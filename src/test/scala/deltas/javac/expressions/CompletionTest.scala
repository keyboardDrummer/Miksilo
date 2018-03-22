package deltas.javac.expressions

import deltas.javac.JavaLanguage
import langserver.types.{CompletionItem, CompletionItemKind}
import lsp.{CompletionList, HumanPosition, LspServerTest}
import org.scalatest.FunSuite
import util.SourceUtils

class CompletionTest extends FunSuite with LspServerTest {

  test("fibonacci") {
    val program = SourceUtils.getJavaTestFileContents("Fibonacci")
    val indexDefinition = getCompletionResultForProgram(JavaLanguage.getJava, program, new HumanPosition(5, 40))
    val item = CompletionItem("fibonacci", kind = Some(CompletionItemKind.Text), insertText = Some("fibonacci"))
    assertResult(CompletionList(isIncomplete = false, Seq(item)))(indexDefinition)
  }
}
