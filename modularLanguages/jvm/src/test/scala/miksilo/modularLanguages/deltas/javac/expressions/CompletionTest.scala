package miksilo.modularLanguages.deltas.javac.expressions

import miksilo.modularLanguages.deltas.javac.JavaLanguage
import miksilo.languageServer.server.{LanguageServerTest, MiksiloLanguageServer}
import miksilo.lspprotocol.lsp.{CompletionList, HumanPosition}
import miksilo.modularLanguages.util.JavaSourceUtils
import org.scalatest.funsuite.AnyFunSuite

class CompletionTest extends AnyFunSuite with LanguageServerTest {

  val server = new MiksiloLanguageServer(JavaLanguage.java)

  test("fibonacci") {
    val program = JavaSourceUtils.getJavaTestFileContents("Fibonacci")
    val indexDefinition = complete(server, program, new HumanPosition(5, 40))
    val item = createCompletionItem("fibonacci")
    assertResult(CompletionList(isIncomplete = false, Seq(item)))(indexDefinition)
  }
}
