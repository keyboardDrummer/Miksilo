package miksilo.lspprotocol.lsp

import java.io.ByteArrayOutputStream

import miksilo.editorParser.parsers.editorParsers.{Position, SourceRange, TextEdit}
import miksilo.lspprotocol.jsonRpc.{JVMMessageReader, JVMMessageWriter, JsonRpcConnection, LSPServer}
import org.scalatest.Assertion
import org.scalatest.funspec.AsyncFunSpec

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Promise}

class LSPServerTest extends AsyncFunSpec {

  case class ServerAndClient(client: LSPClient, server: LSPServer,
                             clientOut: ByteArrayOutputStream,
                             serverOut: ByteArrayOutputStream)

  ignore("can initialize") {

    val languageServer = new TestLanguageServer {}
    val serverAndClient = setupServerAndClient(languageServer)

    val serverOutExpectation =
      """Content-Length: 440
        |
        |{"jsonrpc":"2.0","result":{"capabilities":{"textDocumentSync":1,"hoverProvider":false,"completionProvider":{"resolveProvider":true,"triggerCharacters":[]},"definitionProvider":false,"referencesProvider":false,"documentHighlightProvider":false,"documentSymbolProvider":false,"workspaceSymbolProvider":false,"codeActionProvider":false,"documentFormattingProvider":false,"documentRangeFormattingProvider":false,"renameProvider":false}},"id":0}""".stripMargin
    val clientOutExpectation =
      """Content-Length: 99
        |
        |{"jsonrpc":"2.0","method":"initialize","params":{"rootUri":"someRootUri","capabilities":{}},"id":0}""".stripMargin
    val initializePromise = serverAndClient.client.initialize(InitializeParams(None, Some("someRootUri"), ClientCapabilities()))

    val result = Await.result(initializePromise, Duration.Inf)

    assert(result.capabilities == serverAndClient.server.getCapabilities(ClientCapabilities()))
    assertResult(fixNewlines(clientOutExpectation))(serverAndClient.clientOut.toString)
    assertResult(fixNewlines(serverOutExpectation))(serverAndClient.serverOut.toString)
  }

  ignore("can open document") {
    val document = TextDocumentItem("a","",0,"content")

    val clientDidOpenOutExpectation =
      """Content-Length: 132
        |
        |{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"a","languageId":"","version":0,"text":"content"}}}""".stripMargin

    val clientDidCloseOutExpectation =
      """Content-Length: 88
        |
        |{"jsonrpc":"2.0","method":"textDocument/didClose","params":{"textDocument":{"uri":"a"}}}""".stripMargin

    val opened = Promise[Assertion]()
    val closed = Promise[Assertion]()
    lazy val serverAndClient = setupServerAndClient(languageServer)
    lazy val languageServer: LanguageServer = new TestLanguageServer {
      override def didOpen(parameters: TextDocumentItem): Unit = {
        try {
          opened.success(assertResult(fixNewlines(clientDidOpenOutExpectation))(serverAndClient.clientOut.toString))
          serverAndClient.clientOut.reset()
        } catch {
          case e: Throwable => opened.failure(e)
        }
      }

      override def didClose(parameters: TextDocumentIdentifier): Unit = {
        try {
          closed.success(assertResult(fixNewlines(clientDidCloseOutExpectation))(serverAndClient.clientOut.toString))
        } catch {
          case e: Throwable => closed.failure(e)
        }
      }
    }

    val client = serverAndClient.client
    client.didOpen(document)

    opened.future.flatMap(a => {
      client.didClose(TextDocumentIdentifier(document.uri))
      closed.future
    })
  }

  ignore("can use goto definition") {
    val document = TextDocumentItem("a","",0,"content")
    val request = DocumentPosition(TextDocumentIdentifier(document.uri), Position(0, 0))
    val definitionRange = SourceRange(Position(0, 1), Position(1, 2))

    val languageServer: LanguageServer = new TestLanguageServer with DefinitionProvider {
      override def gotoDefinition(parameters: DocumentPosition): Seq[FileRange] = {
        if (parameters == request)
          return Seq(FileRange(parameters.textDocument.uri, definitionRange))
        Seq.empty
      }
    }

    val serverAndClient = setupServerAndClient(languageServer)
    val client = serverAndClient.client
    val gotoPromise = client.gotoDefinition(request)

    val serverOutExpectation =
      """Content-Length: 121
        |
        |{"jsonrpc":"2.0","result":[{"uri":"a","range":{"start":{"line":0,"character":1},"end":{"line":1,"character":2}}}],"id":0}""".stripMargin
    val clientOutExpectation =
      """Content-Length: 133
        |
        |{"jsonrpc":"2.0","method":"textDocument/definition","params":{"textDocument":{"uri":"a"},"position":{"line":0,"character":0}},"id":0}""".stripMargin
    gotoPromise.map(result => {
      assert(result == Seq(FileRange(document.uri, definitionRange)))
      assertResult(fixNewlines(clientOutExpectation))(serverAndClient.clientOut.toString)
      assertResult(fixNewlines(serverOutExpectation))(serverAndClient.serverOut.toString)
    })
  }

  ignore("can use completion") {
    val document = TextDocumentItem("a","",0, "content")
    val request = DocumentPosition(TextDocumentIdentifier(document.uri), Position(0, 0))

    val languageServer: LanguageServer = new TestLanguageServer with CompletionProvider {
      override def getOptions: CompletionOptions = CompletionOptions(resolveProvider = false, Seq.empty)

      override def complete(request: DocumentPosition): CompletionList =
        CompletionList(isIncomplete = false, Seq(CompletionItem("hello")))
    }

    val serverAndClient = setupServerAndClient(languageServer)
    val client = serverAndClient.client
    val completePromise = client.complete(request)

    val serverOutExpectation =
      """Content-Length: 84
        |
        |{"jsonrpc":"2.0","result":{"isIncomplete":false,"items":[{"label":"hello"}]},"id":0}""".stripMargin
    val clientOutExpectation =
      """Content-Length: 133
        |
        |{"jsonrpc":"2.0","method":"textDocument/completion","params":{"textDocument":{"uri":"a"},"position":{"line":0,"character":0}},"id":0}""".stripMargin
    val result = Await.result(completePromise, Duration.Inf)

    assertResult(Seq(CompletionItem("hello")))(result.items)
    assertResult(fixNewlines(clientOutExpectation))(serverAndClient.clientOut.toString)
    assertResult(fixNewlines(serverOutExpectation))(serverAndClient.serverOut.toString)
  }

  ignore("can use code action") {
    val document = TextDocumentItem("a","",0,"content")
    val someRange = SourceRange(Position(0, 1), Position(1, 2))
    val someOtherRange = SourceRange(Position(0, 0), Position(0, 0))
    val request = CodeActionParams(
      TextDocumentIdentifier(document.uri),
      someRange,
      CodeActionContext(Seq.empty[Diagnostic], None))

    val resultAction = CodeAction("bwoep", "bwap", None, Some(WorkspaceEdit(Map("a" -> Seq(TextEdit(someOtherRange, "jo"))))))
    val languageServer: LanguageServer = new TestLanguageServer with CodeActionProvider {

      override def getCodeActions(parameters: CodeActionParams): Seq[CodeAction] = {
        if (parameters == request) {
          return Seq(resultAction)
        }
        Seq.empty
      }
    }

    val serverAndClient = setupServerAndClient(languageServer)
    val client = serverAndClient.client
    val codeActionPromise = client.codeAction(request)

    val serverOutExpectation =
      """Content-Length: 185
        |
        |{"jsonrpc":"2.0","result":[{"title":"bwoep","kind":"bwap","edit":{"changes":{"a":[{"range":{"start":{"line":0,"character":0},"end":{"line":0,"character":0}},"newText":"jo"}]}}}],"id":0}""".stripMargin
    val clientOutExpectation =
      """Content-Length: 200
        |
        |{"jsonrpc":"2.0","method":"textDocument/codeAction","params":{"textDocument":{"uri":"a"},"range":{"start":{"line":0,"character":1},"end":{"line":1,"character":2}},"context":{"diagnostics":[]}},"id":0}""".stripMargin
    codeActionPromise.map(result => {
      assert(result == Seq(resultAction))
      assertResult(fixNewlines(clientOutExpectation))(serverAndClient.clientOut.toString)
      assertResult(fixNewlines(serverOutExpectation))(serverAndClient.serverOut.toString)
    })
  }

  ignore("can use references") {
    val document = TextDocumentItem("a","",0,"content")
    val request = ReferencesParams(TextDocumentIdentifier(document.uri), Position(0, 0), ReferenceContext(false))
    val referenceRange = SourceRange(Position(0, 1), Position(1, 2))

    val languageServer: LanguageServer = new TestLanguageServer with ReferencesProvider {
      override def references(parameters: ReferencesParams): Seq[FileRange] = {
        if (parameters == request)
          return Seq(FileRange(parameters.textDocument.uri, referenceRange))
        Seq.empty
      }
    }

    val serverAndClient = setupServerAndClient(languageServer)
    val client = serverAndClient.client
    val gotoPromise = client.references(request)

    val serverOutExpectation =
      """Content-Length: 121
        |
        |{"jsonrpc":"2.0","result":[{"uri":"a","range":{"start":{"line":0,"character":1},"end":{"line":1,"character":2}}}],"id":0}""".stripMargin
    val clientOutExpectation =
      """Content-Length: 172
        |
        |{"jsonrpc":"2.0","method":"textDocument/references","params":{"textDocument":{"uri":"a"},"position":{"line":0,"character":0},"context":{"includeDeclaration":false}},"id":0}""".stripMargin
    gotoPromise.map(result => {
      assert(result == Seq(FileRange(document.uri, referenceRange)))
      assertResult(fixNewlines(clientOutExpectation))(serverAndClient.clientOut.toString)
      assertResult(fixNewlines(serverOutExpectation))(serverAndClient.serverOut.toString)
    })
  }

 ignore("can use documentSymbol") {
    val document = TextDocumentItem("a","",0,"content")
    val request = DocumentSymbolParams(TextDocumentIdentifier(document.uri))
    val symbolRange = SourceRange(Position(0, 1), Position(1, 2))
    val symbolInformation = SymbolInformation("someSymbol", SymbolKind.Variable,
      FileRange(request.textDocument.uri, symbolRange), None)

    val languageServer: LanguageServer = new TestLanguageServer with DocumentSymbolProvider {
      override def documentSymbols(params: DocumentSymbolParams): Seq[SymbolInformation] = {
        Seq(symbolInformation)
      }
    }

    val serverAndClient = setupServerAndClient(languageServer)
    val client = serverAndClient.client
    val gotoPromise = client.documentSymbol(request)

    val serverOutExpectation =
      """Content-Length: 164
        |
        |{"jsonrpc":"2.0","result":[{"name":"someSymbol","kind":13,"location":{"uri":"a","range":{"start":{"line":0,"character":1},"end":{"line":1,"character":2}}}}],"id":0}""".stripMargin
    val clientOutExpectation =
      """Content-Length: 101
        |
        |{"jsonrpc":"2.0","method":"textDocument/documentSymbol","params":{"textDocument":{"uri":"a"}},"id":0}""".stripMargin
    gotoPromise.map(result => {
      assert(result == Seq(symbolInformation))
      assertResult(fixNewlines(clientOutExpectation))(serverAndClient.clientOut.toString)
      assertResult(fixNewlines(serverOutExpectation))(serverAndClient.serverOut.toString)
    })
  }

  ignore("can use rename") {
    val document = TextDocumentItem("a","",0,"content")
    val request = RenameParams(TextDocumentIdentifier(document.uri), Position(0, 0), "newName")
    val referenceRange = SourceRange(Position(0, 1), Position(1, 2))

    val expectation = WorkspaceEdit(Map(document.uri -> Seq(
      TextEdit(referenceRange, "newName"),
      TextEdit(referenceRange, "newName")
    )))

    val languageServer: LanguageServer = new TestLanguageServer with RenameProvider {
      override def rename(params: RenameParams): WorkspaceEdit = {
        expectation
      }
    }

    val serverAndClient = setupServerAndClient(languageServer)
    val client = serverAndClient.client
    val gotoPromise = client.rename(request)

    val serverOutExpectation =
      """Content-Length: 245
        |
        |{"jsonrpc":"2.0","result":{"changes":{"a":[{"range":{"start":{"line":0,"character":1},"end":{"line":1,"character":2}},"newText":"newName"},{"range":{"start":{"line":0,"character":1},"end":{"line":1,"character":2}},"newText":"newName"}]}},"id":0}""".stripMargin
    val clientOutExpectation =
      """Content-Length: 149
        |
        |{"jsonrpc":"2.0","method":"textDocument/rename","params":{"textDocument":{"uri":"a"},"position":{"line":0,"character":0},"newName":"newName"},"id":0}""".stripMargin
    gotoPromise.map(result => {
      assert(result == expectation)
      assertResult(fixNewlines(clientOutExpectation))(serverAndClient.clientOut.toString)
      assertResult(fixNewlines(serverOutExpectation))(serverAndClient.serverOut.toString)
    })
  }

  ignore("can receive diagnostics") {
    val document = TextDocumentItem("a","",0,"content")

    val clientOutExpectation =
      """Content-Length: 132
        |
        |{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"a","languageId":"","version":0,"text":"content"}}}""".stripMargin

    val diagnostics = Seq(Diagnostic(SourceRange(HumanPosition(0,1), HumanPosition(0, 5)), Some(2), "Woeps", None, None, Seq.empty))
    val p = Promise[Assertion]()
    val languageClient = new TestLanguageClient {
      override def sendDiagnostics(receivedDiagnostics: PublishDiagnostics): Unit = {
        p.success(assertResult(diagnostics)(receivedDiagnostics.diagnostics))
      }


    }
    val languageServer: LanguageServer = new TestLanguageServer {

      override def getDiagnostics: Seq[Diagnostic] = {
        diagnostics
      }
    }
    val serverAndClient = setupServerAndClient(languageServer, languageClient)

    val client = serverAndClient.client
    client.didChange(DidChangeTextDocumentParams(VersionedTextDocumentIdentifier(document.uri, document.version), Seq.empty))

    p.future
  }

  ignore("merges change notifications") {
    val document = TextDocumentItem("a","",0,"content")

    val diagnostics = Seq(Diagnostic(SourceRange(HumanPosition(0,1), HumanPosition(0, 5)), Some(2), "Woeps", None, None, Seq.empty))
    val promise = Promise[Assertion]()
    val languageClient = new TestLanguageClient {
    }

    val change1 = new TextDocumentContentChangeEvent(None, None, "a")
    val change2 = new TextDocumentContentChangeEvent(None, None, "b")
    val change3 = new TextDocumentContentChangeEvent(None, None, "c")
    val first = DidChangeTextDocumentParams(VersionedTextDocumentIdentifier(document.uri, document.version + 1), Seq(change1))
    val second = DidChangeTextDocumentParams(VersionedTextDocumentIdentifier(document.uri, document.version + 2), Seq(change2))
    val third = DidChangeTextDocumentParams(VersionedTextDocumentIdentifier(document.uri, document.version + 3), Seq(change3))
    val merged = DidChangeTextDocumentParams(VersionedTextDocumentIdentifier(document.uri, document.version + 3), Seq(change2, change3))

    var expectations = List(first, merged)
    val languageServer: LanguageServer = new TestLanguageServer {

      override def didChange(parameters: DidChangeTextDocumentParams): Unit = {
        Thread.sleep(50)
        try {
          assertResult(expectations.head)(parameters)
        } catch {
          case e: Throwable => promise.failure(e)
        }
        expectations = expectations.tail
        if (expectations.isEmpty && !promise.isCompleted) {
            promise.success(assert(true))
        }
        super.didChange(parameters)
      }

      override def getDiagnostics: Seq[Diagnostic] = {
        diagnostics
      }
    }
    val serverAndClient = setupServerAndClient(languageServer, languageClient)
    val client = serverAndClient.client

    client.didChange(first)
    client.complete(DocumentPosition(new TextDocumentIdentifier(document.uri), Position(0, 0)))
    client.didChange(second)
    client.didChange(third)

    promise.future
  }

  def fixNewlines(text: String): String = text.replace("\n","\r\n")

  private def setupServerAndClient(languageServer: LanguageServer, languageClient: LanguageClient = new TestLanguageClient()): ServerAndClient = {
    val clientToServer = new InOutStream()
    val serverToClient = new InOutStream()

    val serverOutCopy = new ByteArrayOutputStream()
    val clientOutCopy = new ByteArrayOutputStream()
    val clientOut = new StreamMultiplexer(Seq(clientToServer.out, clientOutCopy))
    val serverOut = new StreamMultiplexer(Seq(serverToClient.out, serverOutCopy))
    val serverConnection = new JsonRpcConnection(
      new JVMMessageReader(clientToServer.in),
      new JVMMessageWriter(serverOut))
    val server = new LSPServer(languageServer, serverConnection)
    val client = new LSPClient(languageClient, new JsonRpcConnection(
      new JVMMessageReader(serverToClient.in),
      new JVMMessageWriter(clientOut)))
    new Thread(() => server.listen()).start()
    new Thread(() => client.listen()).start()
    ServerAndClient(client, server, clientOutCopy, serverOutCopy)
  }

  class TestLanguageClient extends LanguageClient {
    override def sendDiagnostics(diagnostics: PublishDiagnostics): Unit = {}
    override def trackMetric(name: String, value: Double): Unit = {}
  }

  class TestLanguageServer extends LanguageServer with CompletionProvider {
    override def didOpen(parameters: TextDocumentItem): Unit = {}

    override def didChange(parameters: DidChangeTextDocumentParams): Unit = {
      client.sendDiagnostics(PublishDiagnostics(parameters.textDocument.uri, getDiagnostics))
    }

    def getDiagnostics: Seq[Diagnostic] = Seq.empty

    override def didClose(parameters: TextDocumentIdentifier): Unit = {}

    override def didSave(parameters: DidSaveTextDocumentParams): Unit = {}

    override def initialized(): Unit = {}

    override def initialize(parameters: InitializeParams): Unit = {}

    var client: LanguageClient = _
    override def setClient(client: LanguageClient): Unit = {
      this.client = client
    }

    override def getOptions = new CompletionOptions(true, Seq.empty)

    override def complete(request: DocumentPosition) = new CompletionList(true, Seq.empty)
  }
}
