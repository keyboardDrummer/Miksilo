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
                        |      "name": "keyword.operator",
                        |      "match": ""\"
                        |    },
                        |    {
                        |      "name": "keyword.operator",
                        |      "match": "\\["
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
                        |      "match": "\\]"
                        |    },
                        |    {
                        |      "name": "keyword.operator",
                        |      "match": "\\{"
                        |    },
                        |    {
                        |      "name": "keyword.control",
                        |      "match": "\\b'\\b"
                        |    },
                        |    {
                        |      "name": "keyword.control",
                        |      "match": "\\btrue\\b"
                        |    },
                        |    {
                        |      "name": "keyword.control",
                        |      "match": "\\bfalse\\b"
                        |    },
                        |    {
                        |      "name": "keyword.operator",
                        |      "match": "\\}"
                        |    },
                        |    {
                        |      "name": "keyword.operator",
                        |      "match": ":"
                        |    }
                        |  ]
                        |}"""
    assertResult("""""")(output)
  }
}
