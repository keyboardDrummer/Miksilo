package core.parsers

import editorParsers.LeftRecursiveCorrectingParserWriter
import editorParsers.History
import _root_.core.parsers.strings.CommonStringReaderParser
import _root_.core.textMate.TextMateGeneratingParserWriter

object ParseJson extends CommonStringReaderParser with LeftRecursiveCorrectingParserWriter with TextMateGeneratingParserWriter {

  lazy val boolean = literalOrKeyword("true") | literalOrKeyword("false")
  lazy val arrayParser = literal("[") ~> jsonParser.manySeparated(",", "array element") ~< "]"
  lazy val memberParser = stringLiteral ~< ":" ~ jsonParser
  lazy val objectParser = literal("{", 2 * History.missingInputPenalty) ~>
    memberParser.manySeparated(",", "object member") ~< "}"
  object UnknownExpression {
    override def toString = "unknown"
  }
  lazy val jsonParser: Self[Any] = stringLiteral | objectParser | wholeNumber | arrayParser | boolean |
    Fallback(UnknownExpression, "value")
}
