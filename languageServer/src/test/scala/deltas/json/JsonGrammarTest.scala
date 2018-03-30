package deltas.json

import core.bigrammar.TestLanguageGrammarUtils
import org.scalatest.FunSuite

class JsonGrammarTest extends FunSuite {

  val utils = new TestLanguageGrammarUtils(JsonLanguage.deltas)

  test("parseSimpleJson") {
    val example =
      """{
        |  "x": 3
        |}""".stripMargin

    utils.compareInputWithPrint(example)
  }

  test("optionalComma") {
    val example =
      """{
        |  "x": 3,
        |}""".stripMargin

    val expected =
      """{
        |  "x": 3
        |}""".stripMargin

    val ast = utils.parse(example)
    val printResult = utils.getPrintResult(ast)
    assertResult(expected)(printResult)
  }
}
