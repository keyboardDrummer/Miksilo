package languageServer.lsp

import deltas.javac.JavaLanguage
import languageServer.MiksiloLanguageServer
import org.scalatest.AsyncFunSpec

class LSPServerTest extends AsyncFunSpec {

  val initialize: String = """Content-Length: 304
                             |
                             |{"jsonrpc":"2.0","id":0,"method":"initialize","params":{"rootUri":"file:///local/home/rwillems/workspaces/cloud9-dev/ide-assets/src/AWSCloud9Core/plugins/c9.ide.language.languageServer.lsp/worker/test_files/project","capabilities":{"workspace":{"applyEdit":false},"textDocument":{"definition":true}},"trace":"verbose"}}""".stripMargin.replace("\n", "\r\n")

  val expectedInitializeResult: String = """Content-Length: 440
                                           |
                                           |{"jsonrpc":"2.0","result":{"capabilities":{"textDocumentSync":1,"hoverProvider":false,"completionProvider":{"resolveProvider":false,"triggerCharacters":[]},"definitionProvider":true,"referencesProvider":false,"documentHighlightProvider":false,"documentSymbolProvider":false,"workspaceSymbolProvider":false,"codeActionProvider":false,"documentFormattingProvider":false,"documentRangeFormattingProvider":false,"renameProvider":false}},"id":0}""".
    stripMargin.replace("\n","\r\n")

  it("can initialize") {
    val clientToServer = new InOutStream()
    val serverToClient = new InOutStream()

    val serverConnection = new JsonRpcConnection(clientToServer.in, serverToClient.out)
    val server = new LSPServer(new MiksiloLanguageServer(JavaLanguage.getJava), serverConnection)
    val client = new LSPClient(new JsonRpcConnection(serverToClient.in, clientToServer.out))
    new Thread(() => server.listen()).start()
    new Thread(() => client.listen()).start()

    val initializePromise = client.initialize(InitializeParams(None, "someRootUri", ClientCapabilities()))
    initializePromise.future.map(result =>
      assert(result.capabilities == server.getCapabilities)
    )
  }

//  val document = new TextDocumentIdentifier("jo")
//  val documentItem = new TextDocumentItem("jo","x",1,"")
//
//
//  test("Goto definition through LSP") {
//    val initialize = """Content-Length: 304
//                       |
//                       |{"jsonrpc":"2.0","id":0,"method":"initialize","params":{"rootUri":"file:///local/home/rwillems/workspaces/cloud9-dev/ide-assets/src/AWSCloud9Core/plugins/c9.ide.language.languageServer.lsp/worker/test_files/project","capabilities":{"workspace":{"applyEdit":false},"textDocument":{"definition":true}},"trace":"verbose"}}""".
//      stripMargin.replace("\n", "\r\n")
//
//    val inStream = new MemoryStream()
//    inStream.add(initialize)
//    val outStream = new LspOutStream()
//    val connection = new JsonRpcConnection(inStream, outStream)
//    new MiksiloLanguageServer(CloudFormationLanguage.language, connection)
//
//    val runConnection = new Thread(new Runnable {
//      override def run(): Unit = {
//        connection.listen()
//      }
//    })
//    runConnection.start()
//
//    val result1 = outStream.pop()
//
//    inStream.add("Content-Length: 65\r\n\r\n{\"jsonrpc\":\"2.0\",\"method\":\"initialized\",\"params\":{\"something\":3}}")
//
//    inStream.add("Content-Length: 259\r\n\r\n{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"textDocument/definition\",\"params\":{\"textDocument\":{\"uri\":\"file:///Users/rwillems/Dropbox/Projects/Code/ParticleCompilerSbt/src/test/resources/AutoScalingMultiAZWithNotifications.json\"},\"position\":{\"line\":436,\"character\":35}}}")
//    val secondResult = outStream.pop()
//    val secondExpectation =
//      """Content-Length: 247
//        |
//        |{"jsonrpc":"2.0","result":[{"uri":"file:///Users/rwillems/Dropbox/Projects/Code/ParticleCompilerSbt/src/test/resources/AutoScalingMultiAZWithNotifications.json","range":{"start":{"line":41,"character":4},"end":{"line":41,"character":17}}}],"id":3}""".stripMargin.replace("\n","\r\n")
//    assertResult(secondExpectation)(secondResult)
//  }
//
//  test("goto definition") {
//    val inStream = new MemoryStream()
//    inStream.add(initialize)
//    val outStream = new LspOutStream()
//
//    setupServer(inStream, outStream)
//
//    val initializeResult = outStream.pop()
//    assertResult(expectedInitializeResult)(initializeResult)
//
//    inStream.add("Content-Length: 65\r\n\r\n{\"jsonrpc\":\"2.0\",\"method\":\"initialized\",\"params\":{\"something\":3}}")
//
//    inStream.add("Content-Length: 231\r\n\r\n{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"textDocument/definition\",\"params\":{\"textDocument\":{\"uri\":\"file:///Users/rwillems/Dropbox/Projects/Code/ParticleCompilerSbt/src/test/resources/Fibonacci.java\"},\"position\":{\"line\":5,\"character\":27}}}")
//    val secondResult = outStream.pop()
//    val secondExpectation =
//      """Content-Length: 219
//        |
//        |{"jsonrpc":"2.0","result":[{"uri":"file:///Users/rwillems/Dropbox/Projects/Code/ParticleCompilerSbt/src/test/resources/Fibonacci.java","range":{"start":{"line":0,"character":6},"end":{"line":0,"character":15}}}],"id":1}""".stripMargin.replace("\n","\r\n")
//    assertResult(secondExpectation)(secondResult)
//  }
}
