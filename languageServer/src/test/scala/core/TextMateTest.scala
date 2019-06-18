package core

import org.scalatest.FunSuite
import core.bigrammar.textMate.BiGrammarToTextMate
import _root_.deltas.json.JsonLanguage

class TextMateTest extends FunSuite {

  test("json test") {
    val json = JsonLanguage.language
    var output = BiGrammarToTextMate.toTextMate(json.grammars.root)
    val expectation = """{
                        |  "patterns": [
                        |    {
                        |      "name": "string.quoted.double",
                        |      "match": "\\"([^\\"\\x00-\\x1F\\x7F\\\\]|\\\\[\\\\'\\"bfnrt]|\\\\u[a-fA-F0-9]{4})*\\""
                        |    },
                        |    {
                        |      "name": "string.quoted.single",
                        |      "match": "'[^']*'"
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
                        |      "name": "keyword.control",
                        |      "match": "\\b,\\b"
                        |    },
                        |    {
                        |      "name": "keyword.control",
                        |      "match": "\\bfalse\\b"
                        |    },
                        |    {
                        |      "name": "keyword.control",
                        |      "match": "\\btrue\\b"
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
