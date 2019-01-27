package deltas.javac.expressions

import deltas.javac.JavaLanguage
import langserver.types.{CompletionItem, CompletionItemKind}
import languageServer.lsp.CompletionList
import languageServer.{HumanPosition, LanguageServerTest, MiksiloLanguageServer}
import org.scalatest.FunSuite
import util.SourceUtils

class CompletionTest extends FunSuite with LanguageServerTest {

  val server = new MiksiloLanguageServer(JavaLanguage.getJavaFrontend)

  test("fibonacci") {
    val program = SourceUtils.getJavaTestFileContents("Fibonacci")
    val indexDefinition = complete(server, program, new HumanPosition(5, 40))
    val item = CompletionItem("fibonacci", kind = Some(CompletionItemKind.Text), insertText = Some("fibonacci"))
    assertResult(CompletionList(isIncomplete = false, Seq(item)))(indexDefinition)
  }
}
