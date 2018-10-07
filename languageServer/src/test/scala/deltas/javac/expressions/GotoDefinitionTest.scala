package deltas.javac.expressions

import deltas.javac.JavaLanguage
import deltas.javac.methods.BlockLanguageDelta
import langserver.types.{Location, Range}
import languageServer.{HumanPosition, LanguageServerTest}
import org.scalatest.FunSuite
import util.{SourceUtils, TestLanguageBuilder}

class GotoDefinitionTest extends FunSuite with LanguageServerTest {

  private val blockLanguage = TestLanguageBuilder.buildWithParser(Seq(DropPhases(1), BlockLanguageDelta) ++ JavaLanguage.blockWithVariables)

  test("int variable") {
    val program =
      """int x;
        |x = 3;
      """.stripMargin
    val result = gotoDefinition(blockLanguage.language, program, new HumanPosition(2, 1))
    assertResult(Seq(Location(itemUri, Range(new HumanPosition(1,5), new HumanPosition(1,6)))))(result)
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
    assertResult(Seq(Location(itemUri, Range(new HumanPosition(1,5), new HumanPosition(1,6)))))(xDefinition)

    val yDefinition = gotoDefinition(blockLanguage.language, program, new HumanPosition(4, 8))
    assertResult(Seq(Location(itemUri, Range(new HumanPosition(3,7), new HumanPosition(3,8)))))(yDefinition)
  }

  test("fibonacci") {
    val program = SourceUtils.getJavaTestFileContents("Fibonacci")
    val indexDefinition = gotoDefinition(JavaLanguage.getJava, program, new HumanPosition(10, 16))
    assertResult(Seq(Location(itemUri, Range(new HumanPosition(8,37), new HumanPosition(8,42)))))(indexDefinition)

    val fibonacciDefinition = gotoDefinition(JavaLanguage.getJava, program, new HumanPosition(5, 36))
    val methodRange = Seq(Location(itemUri, Range(new HumanPosition(8, 23), new HumanPosition(8, 32))))
    assertResult(methodRange)(fibonacciDefinition)
  }

  test("assignment") {
    val program = SourceUtils.getJavaTestFileContents("FieldAssignment")
    val myFieldDefinition = gotoDefinition(JavaLanguage.getJava, program, new HumanPosition(11, 9))
    assertResult(Seq(Location(itemUri, Range(new HumanPosition(2,9), new HumanPosition(2,16)))))(myFieldDefinition)
  }
}
