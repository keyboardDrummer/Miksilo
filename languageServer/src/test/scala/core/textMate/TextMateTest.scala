package core.textMate

import core.parsers.editorParsers.{History, LeftRecursiveCorrectingParserWriter}
import core.parsers.strings.CommonStringReaderParser
import org.scalatest.FunSuite

object ParseJsonTextMate extends CommonStringReaderParser
  with LeftRecursiveCorrectingParserWriter with TextMateGeneratingParserWriter {

  lazy val arrayParser = literal("[") ~> jsonParser.manySeparated(",", "array element") ~< "]"
  lazy val memberParser = stringLiteral ~< ":" ~ jsonParser
  lazy val objectParser = literal("{", 2 * History.missingInputPenalty) ~>
    memberParser.manySeparated(",", "object member") ~< "}"
  object UnknownExpression {
    override def toString = "unknown"
  }
  lazy val jsonParser: Self[Any] = stringLiteral | objectParser | wholeNumber | arrayParser |
    Fallback(Succeed(UnknownExpression), "value")
}


class TextMateTest extends FunSuite {

  test("json test") {
    val jsonParser = ParseJsonTextMate.jsonParser
    var output = ParseJsonTextMate.toTextMate(jsonParser)
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
