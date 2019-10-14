package languageServer.lsp

import java.io.ByteArrayOutputStream

import languageServer._
import org.scalatest.{Assertion, AsyncFunSpec}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Promise}

class LSPServerTest extends AsyncFunSpec {

  val initialize: String = """Content-Length: 304
                             |
                             |{"jsonrpc":"2.0","id":0,"method":"initialize","params":{"rootUri":"file:///local/home/rwillems/workspaces/cloud9-dev/ide-assets/src/AWSCloud9Core/plugins/c9.ide.language.languageServer.lsp/worker/test_files/project","capabilities":{"workspace":{"applyEdit":false},"textDocument":{"definition":true}},"trace":"verbose"}}""".stripMargin.replace("\n", "\r\n")

  val expectedInitializeResult: String = """Content-Length: 440
                                           |
                                           |{"jsonrpc":"2.0","result":{"capabilities":{"textDocumentSync":1,"hoverProvider":false,"completionProvider":{"resolveProvider":false,"triggerCharacters":[]},"definitionProvider":true,"referencesProvider":false,"documentHighlightProvider":false,"documentSymbolProvider":false,"workspaceSymbolProvider":false,"codeActionProvider":false,"documentFormattingProvider":false,"documentRangeFormattingProvider":false,"renameProvider":false}},"id":0}""".
    stripMargin.replace("\n","\r\n")

  case class ServerAndClient(client: LSPClient, server: LSPServer,
                             clientOut: ByteArrayOutputStream,
                             serverOut: ByteArrayOutputStream)

  ignore("can initialize") {

    val languageServer = new TestLanguageServer {}
    val serverAndClient = setupServerAndClient(languageServer)

    val serverOutExpectation =
      """Content-Length: 371
        |
        |{"jsonrpc":"2.0","result":{"capabilities":{"textDocumentSync":1,"hoverProvider":false,"definitionProvider":false,"referencesProvider":false,"documentHighlightProvider":false,"documentSymbolProvider":false,"workspaceSymbolProvider":false,"codeActionProvider":false,"documentFormattingProvider":false,"documentRangeFormattingProvider":false,"renameProvider":false}},"id":0}""".stripMargin
    val clientOutExpectation =
      """Content-Length: 99
        |
        |{"jsonrpc":"2.0","method":"initialize","params":{"rootUri":"someRootUri","capabilities":{}},"id":0}""".stripMargin
    val initializePromise = serverAndClient.client.initialize(InitializeParams(None, "someRootUri", ClientCapabilities()))

    val result = Await.result(initializePromise.future, Duration.Inf)

    assert(result.capabilities == serverAndClient.server.getCapabilities(ClientCapabilities()))
    assertResult(fixNewlines(clientOutExpectation))(serverAndClient.clientOut.toString)
    assertResult(fixNewlines(serverOutExpectation))(serverAndClient.serverOut.toString)
  }

  ignore("can open document") {
    val document = TextDocumentItem("a","",0,"content")

    val clientOutExpectation =
      """Content-Length: 132
        |
        |{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"a","languageId":"","version":0,"text":"content"}}}""".stripMargin

    val p = Promise[Assertion]()
    lazy val serverAndClient = setupServerAndClient(languageServer)
    lazy val languageServer: LanguageServer = new TestLanguageServer {
      override def didOpen(parameters: TextDocumentItem): Unit = {
        try {
          p.success(assertResult(fixNewlines(clientOutExpectation))(serverAndClient.clientOut.toString))
        } catch {
          case e: Throwable => p.failure(e)
        }
      }
    }

    val client = serverAndClient.client
    client.didOpen(document)

    p.future
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
    gotoPromise.future.map(result => {
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
    val result = Await.result(completePromise.future, Duration.Inf)

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
    codeActionPromise.future.map(result => {
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
    gotoPromise.future.map(result => {
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
    gotoPromise.future.map(result => {
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
    gotoPromise.future.map(result => {
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

    val diagnostics = Seq(Diagnostic(SourceRange(HumanPosition(0,1), HumanPosition(0, 5)), Some(2), "Woeps", None, None))
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


  def fixNewlines(text: String): String = text.replace("\n","\r\n")

  private def setupServerAndClient(languageServer: LanguageServer, languageClient: LanguageClient = new TestLanguageClient()): ServerAndClient = {
    val clientToServer = new InOutStream()
    val serverToClient = new InOutStream()

    val serverOutCopy = new ByteArrayOutputStream()
    val clientOutCopy = new ByteArrayOutputStream()
    val clientOut = new StreamMultiplexer(Seq(clientToServer.out, clientOutCopy))
    val serverOut = new StreamMultiplexer(Seq(serverToClient.out, serverOutCopy))
    val serverConnection = new JsonRpcConnection(clientToServer.in, serverOut)
    val server = new LSPServer(languageServer, serverConnection)
    val client = new LSPClient(languageClient, new JsonRpcConnection(serverToClient.in, clientOut))
    new Thread(() => server.listen()).start()
    new Thread(() => client.listen()).start()
    ServerAndClient(client, server, clientOutCopy, serverOutCopy)
  }

  class TestLanguageClient extends LanguageClient {
    override def sendDiagnostics(diagnostics: PublishDiagnostics): Unit = {}
  }

  class TestLanguageServer extends LanguageServer {
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
  }
}
