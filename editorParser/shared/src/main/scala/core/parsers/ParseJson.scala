package core.parsers

import _root_.core.parsers.strings.{CommonStringReaderParser, WhitespaceParserWriter}
import _root_.core.parsers.editorParsers.{History, LeftRecursiveCorrectingParserWriter}

object ParseJson extends CommonStringReaderParser with LeftRecursiveCorrectingParserWriter with WhitespaceParserWriter {

  lazy val arrayParser = literal("[") ~> jsonParser.manySeparated(",", "array element") ~< "]"
  lazy val memberParser = stringLiteral ~< ":" ~ jsonParser
  lazy val objectParser = literal("{", 2 * History.missingInputPenalty) ~>
    memberParser.manySeparated(",", "object member") ~< "}"
  object UnknownExpression {
    override def toString = "unknown"
  }
  lazy val jsonParser: Parser[Any] = stringLiteral | objectParser // | wholeNumber |  // arrayParser |    Fallback(Succeed(UnknownExpression), "value")
}
