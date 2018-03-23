package deltas.javac.expressions

import core.deltas.Delta
import core.language.node.SourceRange
import deltas.javac.JavaLanguage
import deltas.javac.methods.BlockLanguageDelta
import lsp._
import org.scalatest.FunSuite
import util.SourceUtils

class GotoDefinitionTest extends FunSuite with LspServerTest {

  private val blockLanguage = Delta.buildLanguage(Seq(DropPhases(1), BlockLanguageDelta) ++ JavaLanguage.blockWithVariables)

  test("int variable") {
    val program =
      """int x;
        |x = 3;
      """.stripMargin
    val result = getDefinitionResultForProgram(blockLanguage, program, new HumanPosition(2, 1))
    assertResult(SourceRange(new HumanPosition(1,5), new HumanPosition(1,6)))(result)
  }

  test("defined inside if") {
    val program =
      """int x;
        |if (true) {
        |  int y = 2;
        |  x += y;
        |}
      """.stripMargin
    val xDefinition = getDefinitionResultForProgram(blockLanguage, program, new HumanPosition(4, 3))
    assertResult(SourceRange(new HumanPosition(1,5), new HumanPosition(1,6)))(xDefinition)

    val yDefinition = getDefinitionResultForProgram(blockLanguage, program, new HumanPosition(4, 8))
    assertResult(SourceRange(new HumanPosition(3,7), new HumanPosition(3,8)))(yDefinition)
  }

  test("fibonacci") {
    val program = SourceUtils.getJavaTestFileContents("Fibonacci")
    val indexDefinition = getDefinitionResultForProgram(JavaLanguage.getJava, program, new HumanPosition(10, 16))
    assertResult(SourceRange(new HumanPosition(8,37), new HumanPosition(8,42)))(indexDefinition)

    val fibonacciDefinition = getDefinitionResultForProgram(JavaLanguage.getJava, program, new HumanPosition(5, 36))
    val methodRange = SourceRange(new HumanPosition(8, 23), new HumanPosition(8, 32))
    assertResult(methodRange)(fibonacciDefinition)
  }

  test("assignment") {
    val program = SourceUtils.getJavaTestFileContents("FieldAssignment")
    val myFieldDefinition = getDefinitionResultForProgram(JavaLanguage.getJava, program, new HumanPosition(11, 9))
    assertResult(SourceRange(new HumanPosition(2,9), new HumanPosition(2,16)))(myFieldDefinition)
  }
}
