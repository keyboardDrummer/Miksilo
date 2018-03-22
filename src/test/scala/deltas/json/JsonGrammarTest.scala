package deltas.json

import core.bigrammar.TestLanguageGrammarUtils
import org.scalatest.FunSuite
import util.SourceUtils

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

  test("parse vfsServiceTemplate") {
    val source = SourceUtils.getTestFileContents("vfsServiceTemplate.stpl")
    utils.parse(source)
  }
}
