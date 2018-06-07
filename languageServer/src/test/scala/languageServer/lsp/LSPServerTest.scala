package languageServer.lsp

import java.io.ByteArrayOutputStream

import langserver.types._
import languageServer._
import org.scalatest.{Assertion, AsyncFunSpec}

import scala.concurrent.Promise

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

  it("can initialize") {

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
    initializePromise.future.map(result => {
      assert(result.capabilities == serverAndClient.server.getCapabilities)
      assertResult(fixNewlines(clientOutExpectation))(serverAndClient.clientOut.toString)
      assertResult(fixNewlines(serverOutExpectation))(serverAndClient.serverOut.toString)
    })
  }

  it("can open document") {
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

  it("can use goto definition") {
    val document = TextDocumentItem("a","",0,"content")
    val request = DocumentPosition(TextDocumentIdentifier(document.uri), Position(0, 0))
    val definitionRange = Range(Position(0, 1), Position(1, 2))

    val languageServer: LanguageServer = new TestLanguageServer with DefinitionProvider {
      override def gotoDefinition(parameters: DocumentPosition): Seq[Location] = {
        if (parameters == request)
          return Seq(Location(parameters.textDocument.uri, definitionRange))
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
      assert(result == Seq(Location(document.uri, definitionRange)))
      assertResult(fixNewlines(clientOutExpectation))(serverAndClient.clientOut.toString)
      assertResult(fixNewlines(serverOutExpectation))(serverAndClient.serverOut.toString)
    })
  }

  it("can use completion") {
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
    completePromise.future.map(result => {
      assert(result.items == Seq(CompletionItem("hello")))
      assertResult(fixNewlines(clientOutExpectation))(serverAndClient.clientOut.toString)
      assertResult(fixNewlines(serverOutExpectation))(serverAndClient.serverOut.toString)
    })

  }

  it("can use references") {
    val document = TextDocumentItem("a","",0,"content")
    val request = ReferencesParams(TextDocumentIdentifier(document.uri), Position(0, 0), ReferenceContext(false))
    val referenceRange = Range(Position(0, 1), Position(1, 2))

    val languageServer: LanguageServer = new TestLanguageServer with ReferencesProvider {
      override def references(parameters: ReferencesParams): Seq[Location] = {
        if (parameters == request)
          return Seq(Location(parameters.textDocument.uri, referenceRange))
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
      assert(result == Seq(Location(document.uri, referenceRange)))
      assertResult(fixNewlines(clientOutExpectation))(serverAndClient.clientOut.toString)
      assertResult(fixNewlines(serverOutExpectation))(serverAndClient.serverOut.toString)
    })
  }

  it("can receive diagnostics") {
    val document = TextDocumentItem("a","",0,"content")

    val clientOutExpectation =
      """Content-Length: 132
        |
        |{"jsonrpc":"2.0","method":"textDocument/didOpen","params":{"textDocument":{"uri":"a","languageId":"","version":0,"text":"content"}}}""".stripMargin

    val diagnostics = Seq(Diagnostic(Range(HumanPosition(0,1), HumanPosition(0, 5)),Some(2), None, None, "Woeps"))
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
