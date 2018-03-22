package deltas.json

import core.bigrammar.TestLanguageGrammarUtils
import org.scalatest.FunSuite
import util.SourceUtils

class JsonGrammar extends FunSuite {

  val utils = new TestLanguageGrammarUtils(JsonLanguage.deltas)

  test("parseSimpleJson") {
    val example =
      """{
        |  "x": 3
        |}""".stripMargin

    utils.compareInputWithPrint(example)
  }

  test("parse vfsServiceTemplate") {
    val source = SourceUtils.getTestFileContents("vfsServiceTemplate.stpl")
    utils.parse(source)
  }
}
