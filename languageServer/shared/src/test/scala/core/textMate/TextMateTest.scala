package core.textMate

import languages.json.JsonParser
import org.scalatest.funsuite.AnyFunSuite

class TextMateTest extends AnyFunSuite {

  test("json test") {
    val jsonParser = JsonParser.valueParser
    val output = GenerateTextMateGrammar.toTextMate(JsonParser)(jsonParser)
    val expectation = """{
                        |  "patterns": [
                        |    {
                        |      "name": "string.quoted.double",
                        |      "match": "\"([^\"\\x00-\\x1F\\x7F\\\\]|\\\\[\\\\'\"bfnrt]|\\\\u[a-fA-F0-9]{4})*\""
                        |    },
                        |    {
                        |      "name": "keyword.operator",
                        |      "match": ","
                        |    },
                        |    {
                        |      "name": "constant.numeric",
                        |      "match": "-?\\d+"
                        |    },
                        |    {
                        |      "name": "keyword.operator",
                        |      "match": ":"
                        |    },
                        |    {
                        |      "name": "keyword.operator",
                        |      "match": "\\["
                        |    },
                        |    {
                        |      "name": "keyword.operator",
                        |      "match": "\\]"
                        |    },
                        |    {
                        |      "name": "keyword.operator",
                        |      "match": "\\{"
                        |    },
                        |    {
                        |      "name": "keyword.operator",
                        |      "match": "\\}"
                        |    }
                        |  ]
                        |}""".stripMargin
    assertResult(expectation)(output)
  }
}
