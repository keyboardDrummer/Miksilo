package core.textMate

import core.parsers.ParseJson
import org.scalatest.FunSuite

class TextMateTest extends FunSuite {

//  test("smithy test") {
//    val smithy = SmithyLanguage.language
//    var output = BiGrammarToTextMate.toTextMate(LanguageGrammars.grammars.get(smithy).root)
//    val expectation = """{
//                        |  "patterns": [
//                        |    {
//                        |      "name": "string.quoted.double",
//                        |      "match": "\\"([^\\"\\x00-\\x1F\\x7F\\\\]|\\\\[\\\\'\\"bfnrt]|\\\\u[a-fA-F0-9]{4})*\\""
//                        |    },
//                        |    {
//                        |      "name": "string.quoted.double",
//                        |      "match": "\\"([^\\"\\x00-\\x1F\\x7F\\\\]|\\\\[\\\\'\\"bfnrt]|\\\\u[a-fA-F0-9]{4})*\\"|\\[_a\\-zA\\-Z\\]\\[_a\\-zA\\-Z0\\-9\\]\\*"
//                        |    },
//                        |    {
//                        |      "name": "keyword.operator",
//                        |      "match": "#"
//                        |    },
//                        |    {
//                        |      "name": "string.quoted.single",
//                        |      "match": "'[^']*'"
//                        |    },
//                        |    {
//                        |      "name": "keyword.operator",
//                        |      "match": ","
//                        |    },
//                        |    {
//                        |      "name": "constant.numeric",
//                        |      "match": "-?\\d+"
//                        |    },
//                        |    {
//                        |      "name": "comment.line.double-slash",
//                        |      "match": "//[^\\n]*\\n"
//                        |    },
//                        |    {
//                        |      "name": "keyword.operator",
//                        |      "match": ":"
//                        |    },
//                        |    {
//                        |      "name": "keyword.operator",
//                        |      "match": "@"
//                        |    },
//                        |    {
//                        |      "name": "variable",
//                        |      "match": "[A-Za-z][A-Za-z0-9_]*"
//                        |    },
//                        |    {
//                        |      "name": "keyword.operator",
//                        |      "match": "\\$"
//                        |    },
//                        |    {
//                        |      "name": "keyword.operator",
//                        |      "match": "\\("
//                        |    },
//                        |    {
//                        |      "name": "keyword.operator",
//                        |      "match": "\\)"
//                        |    },
//                        |    {
//                        |      "name": "keyword.operator",
//                        |      "match": "\\-\\>"
//                        |    },
//                        |    {
//                        |      "name": "keyword.operator",
//                        |      "match": "\\."
//                        |    },
//                        |    {
//                        |      "name": "keyword.operator",
//                        |      "match": "\\["
//                        |    },
//                        |    {
//                        |      "name": "keyword.operator",
//                        |      "match": "\\]"
//                        |    },
//                        |    {
//                        |      "name": "keyword.control",
//                        |      "match": "\\b,\\b"
//                        |    },
//                        |    {
//                        |      "name": "variable",
//                        |      "match": "\\b[A-Za-z][A-Za-z0-9_]*\\b"
//                        |    },
//                        |    {
//                        |      "name": "keyword.control",
//                        |      "match": "\\bbigDecimal\\b"
//                        |    },
//                        |    {
//                        |      "name": "keyword.control",
//                        |      "match": "\\bbigInteger\\b"
//                        |    },
//                        |    {
//                        |      "name": "keyword.control",
//                        |      "match": "\\bblob\\b"
//                        |    },
//                        |    {
//                        |      "name": "keyword.control",
//                        |      "match": "\\bboolean\\b"
//                        |    },
//                        |    {
//                        |      "name": "keyword.control",
//                        |      "match": "\\bbyte\\b"
//                        |    },
//                        |    {
//                        |      "name": "keyword.control",
//                        |      "match": "\\bdouble\\b"
//                        |    },
//                        |    {
//                        |      "name": "keyword.control",
//                        |      "match": "\\berrors\\b"
//                        |    },
//                        |    {
//                        |      "name": "keyword.control",
//                        |      "match": "\\bfalse\\b"
//                        |    },
//                        |    {
//                        |      "name": "keyword.control",
//                        |      "match": "\\bfloat\\b"
//                        |    },
//                        |    {
//                        |      "name": "keyword.control",
//                        |      "match": "\\binteger\\b"
//                        |    },
//                        |    {
//                        |      "name": "keyword.control",
//                        |      "match": "\\blist\\b"
//                        |    },
//                        |    {
//                        |      "name": "keyword.control",
//                        |      "match": "\\blong\\b"
//                        |    },
//                        |    {
//                        |      "name": "keyword.control",
//                        |      "match": "\\bmember\\b"
//                        |    },
//                        |    {
//                        |      "name": "keyword.control",
//                        |      "match": "\\bnamespace\\b"
//                        |    },
//                        |    {
//                        |      "name": "keyword.control",
//                        |      "match": "\\boperation\\b"
//                        |    },
//                        |    {
//                        |      "name": "keyword.control",
//                        |      "match": "\\bresource\\b"
//                        |    },
//                        |    {
//                        |      "name": "keyword.control",
//                        |      "match": "\\bservice\\b"
//                        |    },
//                        |    {
//                        |      "name": "keyword.control",
//                        |      "match": "\\bshort\\b"
//                        |    },
//                        |    {
//                        |      "name": "keyword.control",
//                        |      "match": "\\bstring\\b"
//                        |    },
//                        |    {
//                        |      "name": "keyword.control",
//                        |      "match": "\\bstructure\\b"
//                        |    },
//                        |    {
//                        |      "name": "keyword.control",
//                        |      "match": "\\btimestamp\\b"
//                        |    },
//                        |    {
//                        |      "name": "keyword.control",
//                        |      "match": "\\btrait\\b"
//                        |    },
//                        |    {
//                        |      "name": "keyword.control",
//                        |      "match": "\\btrue\\b"
//                        |    },
//                        |    {
//                        |      "name": "keyword.operator",
//                        |      "match": "\\{"
//                        |    },
//                        |    {
//                        |      "name": "keyword.operator",
//                        |      "match": "\\}"
//                        |    }
//                        |  ]
//                        |}""".stripMargin
//    assertResult(expectation)(output)
//
//    val addition = """  "information_for_contributors": [],
//                     |  "version": "",
//                     |  "name": "Smithy",
//                     |  "scopeName": "source.smithy",
//                     |  """.stripMargin
//    val pastable = output.replaceFirst("\n  ", "\n" + addition)
//  }

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
