package cloudformation

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets

import core.language.node.SourceRange
import deltas.cloudformation.CloudFormationLanguage
import langserver.types._
import lsp._
import org.scalatest.FunSuite
import util.SourceUtils

class CloudFormationTest extends FunSuite with LspServerTest {

  test("Goto definition") {
    val program = SourceUtils.getTestFileContents("CloudFormationParameterReference.stpl")
    val indexDefinition = getDefinitionResultForProgram(CloudFormationLanguage.language, program, new Position(46, 24))
    assertResult(SourceRange(new Position(5,5), new Position(5,25)))(indexDefinition)
  }

  test("Code completion") {
    val program = SourceUtils.getTestFileContents("CloudFormationParameterReference.stpl")
    val result = getCompletionResultForProgram(CloudFormationLanguage.language, program, new HumanPosition(17, 18))
    val item = CompletionItem("AvailabilityZones", kind = Some(CompletionItemKind.Text))
    assertResult(CompletionList(isIncomplete = false, Seq(item)))(result)
  }

  test("can it initialize") {
    var input = """Content-Length: 304
                  |
                  |{"jsonrpc":"2.0","id":0,"method":"initialize","params":{"rootUri":"file:///local/home/rwillems/workspaces/cloud9-dev/ide-assets/src/AWSCloud9Core/plugins/c9.ide.language.lsp/worker/test_files/project","capabilities":{"workspace":{"applyEdit":false},"textDocument":{"definition":true}},"trace":"verbose"}}""".stripMargin
    input = input.replace("\n", "\r\n")
    val inStream = new MemoryStream()
    inStream.add(input.getBytes(StandardCharsets.UTF_8))
    val outStream = new ByteArrayOutputStream()
    val connection = new JsonRpcConnection(inStream, outStream)
    new MiksiloLanguageServer(CloudFormationLanguage.language, connection)

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
    val expectation = """Content-Length: 370
                        |
                        |{"jsonrpc":"2.0","result":{"capabilities":{"textDocumentSync":1,"hoverProvider":false,"definitionProvider":true,"referencesProvider":false,"documentHighlightProvider":false,"documentSymbolProvider":false,"workspaceSymbolProvider":false,"codeActionProvider":false,"documentFormattingProvider":false,"documentRangeFormattingProvider":false,"renameProvider":false}},"id":0}""".
      stripMargin.replace("\n","\r\n")
    assertResult(expectation)(result)

    add("Content-Length: 65\r\n\r\n{\"jsonrpc\":\"2.0\",\"method\":\"initialized\",\"params\":{\"something\":3}}")

    add("Content-Length: 255\r\n\r\n{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"textDocument/definition\",\"params\":{\"textDocument\":{\"uri\":\"file:///Users/rwillems/Dropbox/Projects/Code/ParticleCompilerSbt/src/test/resources/CloudFormationParameterReference.stpl\"},\"position\":{\"line\":45,\"character\":34}}}")
    val secondResult = pop()
    val secondExpectation =
      """Content-Length: 242
        |
        |{"jsonrpc":"2.0","result":[{"uri":"file:///Users/rwillems/Dropbox/Projects/Code/ParticleCompilerSbt/src/test/resources/CloudFormationParameterReference.stpl","range":{"start":{"line":4,"character":4},"end":{"line":4,"character":24}}}],"id":3}""".stripMargin.replace("\n","\r\n")
    assertResult(secondExpectation)(secondResult)
  }
}
