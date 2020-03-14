package deltas.javac.expressions

import deltas.javac.JavaLanguage
import languageServer.{LanguageServerTest, MiksiloLanguageServer}
import miksilo.lspprotocol.lsp.{CompletionList, HumanPosition}
import org.scalatest.funsuite.AnyFunSuite
import util.JavaSourceUtils

class CompletionTest extends AnyFunSuite with LanguageServerTest {

  val server = new MiksiloLanguageServer(JavaLanguage.java)

  test("fibonacci") {
    val program = JavaSourceUtils.getJavaTestFileContents("Fibonacci")
    val indexDefinition = complete(server, program, new HumanPosition(5, 40))
    val item = createCompletionItem("fibonacci")
    assertResult(CompletionList(isIncomplete = false, Seq(item)))(indexDefinition)
  }
}
