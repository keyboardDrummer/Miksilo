package core.textMate

import core.parsers.ParseJson
import org.scalatest.FunSuite

class TextMateTest extends FunSuite {

  test("json test") {
    val jsonParser = ParseJson.jsonParser
    var output = ParseJson.toTextMate(jsonParser)
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
