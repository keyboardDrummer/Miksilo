package core.parsers

import editorParsers.LeftRecursiveCorrectingParserWriter
import editorParsers.History

object ParseJson extends CommonStringReaderParser with LeftRecursiveCorrectingParserWriter {

  lazy val arrayParser = literal("[") ~> jsonParser.manySeparated(",", "array element") ~< "]"
  lazy val memberParser = stringLiteral ~< ":" ~ jsonParser
  lazy val objectParser = literal("{", 2 * History.missingInputPenalty) ~>
    memberParser.manySeparated(",", "object member") ~< "}"
  object UnknownExpression {
    override def toString = "unknown"
  }
  lazy val jsonParser: Self[Any] = stringLiteral | objectParser | wholeNumber | arrayParser |
    Fallback(UnknownExpression, "value")
}
