package lsp

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets

import org.scalatest.FunSuite


class LanguageServerTest extends FunSuite {

  test("can it initialize") {
    var input = """Content-Length: 304
                  |
                  |{"jsonrpc":"2.0","id":0,"method":"initialize","params":{"rootUri":"file:///local/home/rwillems/workspaces/cloud9-dev/ide-assets/src/AWSCloud9Core/plugins/c9.ide.language.lsp/worker/test_files/project","capabilities":{"workspace":{"applyEdit":false},"textDocument":{"definition":true}},"trace":"verbose"}}""".stripMargin
    input = input.replace("\n", "\r\n")
    val inStream = new MemoryStream()
    inStream.add(input.getBytes(StandardCharsets.UTF_8))
    val outStream = new ByteArrayOutputStream()
    val connection = new JsonRpcConnection(inStream, outStream)
    val languageServer = new LanguageServer(connection)

    val runConnection = new Thread(new Runnable {
      override def run(): Unit = {
        connection.start()
      }
    })
    runConnection.start()

    while(outStream.toString.isEmpty) {
      Thread.sleep(5)
    }

    val result = outStream.toString
    val expectation = """Content-Length: 371
                        |
                        |{"jsonrpc":"2.0","result":{"capabilities":{"textDocumentSync":1,"hoverProvider":false,"definitionProvider":false,"referencesProvider":false,"documentHighlightProvider":false,"documentSymbolProvider":false,"workspaceSymbolProvider":false,"codeActionProvider":false,"documentFormattingProvider":false,"documentRangeFormattingProvider":false,"renameProvider":false}},"id":0}""".
      stripMargin.replace("\n","\r\n")
    assertResult(expectation)(result)
  }
}
