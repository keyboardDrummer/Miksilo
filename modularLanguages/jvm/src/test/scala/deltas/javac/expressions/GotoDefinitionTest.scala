package deltas.javac.expressions

import core.parsers.editorParsers.SourceRange
import deltas.javac.JavaLanguage
import deltas.javac.methods.BlockLanguageDelta
import languageServer.LanguageServerTest
import lsp.{FileRange, HumanPosition}
import org.scalatest.funsuite.AnyFunSuite
import util.{JavaSourceUtils, TestLanguageBuilder}

class GotoDefinitionTest extends AnyFunSuite with LanguageServerTest {

  private val blockLanguage = TestLanguageBuilder.buildWithParser(Seq(
    DropPhases(1), BlockLanguageDelta) ++
    JavaLanguage.blockWithVariables)

  test("int variable") {
    val program =
      """int x;
        |x = 3;
      """.stripMargin
    val result = gotoDefinition(blockLanguage.language, program, new HumanPosition(2, 1))
    assertResult(SourceRange(new HumanPosition(1,5), new HumanPosition(1,6)))(result.head.range)
  }

  test("defined inside if") {
    val program =
      """int x;
        |if (true) {
        |  int y = 2;
        |  x += y;
        |}
      """.stripMargin
    val xDefinition = gotoDefinition(blockLanguage.language, program, new HumanPosition(4, 3))
    assertResult(SourceRange(new HumanPosition(1,5), new HumanPosition(1,6)))(xDefinition.head.range)

    val yDefinition = gotoDefinition(blockLanguage.language, program, new HumanPosition(4, 8))
    assertResult(SourceRange(new HumanPosition(3,7), new HumanPosition(3,8)))(yDefinition.head.range)
  }

  test("fibonacci") {
    val program = JavaSourceUtils.getJavaTestFileContents("Fibonacci")
    val indexDefinition = gotoDefinition(JavaLanguage.java, program, new HumanPosition(10, 16))
    assertResult(SourceRange(new HumanPosition(8,37), new HumanPosition(8,42)))(indexDefinition.head.range)

    val fibonacciDefinition = gotoDefinition(JavaLanguage.java, program, new HumanPosition(5, 36))
    val methodRange = SourceRange(new HumanPosition(8, 23), new HumanPosition(8, 32))
    assertResult(methodRange)(fibonacciDefinition.head.range)
  }

  test("assignment") {
    val program = JavaSourceUtils.getJavaTestFileContents("FieldAssignment")
    val myFieldDefinition = gotoDefinition(JavaLanguage.java, program, new HumanPosition(11, 9))
    assertResult(SourceRange(new HumanPosition(2,9), new HumanPosition(2,16)))(myFieldDefinition.head.range)
  }
}
