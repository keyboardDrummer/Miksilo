package cloudformation

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets

import core.bigrammar.TestLanguageGrammarUtils
import core.language.node.SourceRange
import deltas.cloudformation.CloudFormationLanguage
import langserver.types._
import lsp._
import org.scalatest.FunSuite
import util.SourceUtils

class CloudFormationTest extends FunSuite with LspServerTest {

  test("Goto definition") {
    val program = SourceUtils.getTestFileContents("AutoScalingMultiAZWithNotifications.json")
    val result = getDefinitionResultForProgram(CloudFormationLanguage.language, program, new HumanPosition(437, 36))
    assertResult(SourceRange(new HumanPosition(42,5), new HumanPosition(42,18)))(result)
  }

  test("Code completion") {
    val program = SourceUtils.getTestFileContents("AutoScalingMultiAZWithNotifications.json")
    val result = getCompletionResultForProgram(CloudFormationLanguage.language, program, new HumanPosition(214, 14))
    val item = CompletionItem("\"Subscription\"", kind = Some(CompletionItemKind.Text), insertText = Some("cription\""))
    assertResult(CompletionList(isIncomplete = false, Seq(item)))(result)
  }

  test("Goto definition through LSP") {
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

    pop()

    add("Content-Length: 65\r\n\r\n{\"jsonrpc\":\"2.0\",\"method\":\"initialized\",\"params\":{\"something\":3}}")

    add("Content-Length: 259\r\n\r\n{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"textDocument/definition\",\"params\":{\"textDocument\":{\"uri\":\"file:///Users/rwillems/Dropbox/Projects/Code/ParticleCompilerSbt/src/test/resources/AutoScalingMultiAZWithNotifications.json\"},\"position\":{\"line\":436,\"character\":35}}}")
    val secondResult = pop()
    val secondExpectation =
      """Content-Length: 247
        |
        |{"jsonrpc":"2.0","result":[{"uri":"file:///Users/rwillems/Dropbox/Projects/Code/ParticleCompilerSbt/src/test/resources/AutoScalingMultiAZWithNotifications.json","range":{"start":{"line":41,"character":4},"end":{"line":41,"character":17}}}],"id":3}""".stripMargin.replace("\n","\r\n")
    assertResult(secondExpectation)(secondResult)
  }

  test("parse vfsServiceTemplate") {
    val utils = new TestLanguageGrammarUtils(CloudFormationLanguage.deltas)
    val source = SourceUtils.getTestFileContents("AutoScalingMultiAZWithNotifications.json")
    utils.parse(source)
  }
}
