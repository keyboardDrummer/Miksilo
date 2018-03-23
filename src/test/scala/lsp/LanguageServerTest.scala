package lsp

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets

import deltas.javac.JavaLanguage
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
    new MiksiloLanguageServer(JavaLanguage.getJava, connection)

    def add(value: String) = inStream.add(value.getBytes(StandardCharsets.UTF_8))

    def pop(): String = {
      while(outStream.toString.isEmpty) {
        Thread.sleep(5)
      }

      val result = outStream.toString
      outStream.reset()
      result
    }

    val runConnection = new Thread(new Runnable {
      override def run(): Unit = {
        connection.start()
      }
    })
    runConnection.start()

    val result = pop()
    val expectation = """Content-Length: 440
                        |
                        |{"jsonrpc":"2.0","result":{"capabilities":{"textDocumentSync":1,"hoverProvider":false,"completionProvider":{"resolveProvider":false,"triggerCharacters":[]},"definitionProvider":true,"referencesProvider":false,"documentHighlightProvider":false,"documentSymbolProvider":false,"workspaceSymbolProvider":false,"codeActionProvider":false,"documentFormattingProvider":false,"documentRangeFormattingProvider":false,"renameProvider":false}},"id":0}""".
      stripMargin.replace("\n","\r\n")
    assertResult(expectation)(result)

    add("Content-Length: 65\r\n\r\n{\"jsonrpc\":\"2.0\",\"method\":\"initialized\",\"params\":{\"something\":3}}")

    add("Content-Length: 231\r\n\r\n{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"textDocument/definition\",\"params\":{\"textDocument\":{\"uri\":\"file:///Users/rwillems/Dropbox/Projects/Code/ParticleCompilerSbt/src/test/resources/Fibonacci.java\"},\"position\":{\"line\":5,\"character\":27}}}")
    val secondResult = pop()
    val secondExpectation =
      """Content-Length: 219
        |
        |{"jsonrpc":"2.0","result":[{"uri":"file:///Users/rwillems/Dropbox/Projects/Code/ParticleCompilerSbt/src/test/resources/Fibonacci.java","range":{"start":{"line":0,"character":6},"end":{"line":0,"character":15}}}],"id":1}""".stripMargin.replace("\n","\r\n")
    assertResult(secondExpectation)(secondResult)
  }
}
