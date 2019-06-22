package core.parsers

import editorParsers.LeftRecursiveCorrectingParserWriter
import editorParsers.History

object ParseJson extends CommonStringReaderParser with LeftRecursiveCorrectingParserWriter {

  lazy val arrayParser = Literal("[") ~> jsonParser.manySeparated(",", "array element") ~< "]"
  lazy val memberParser = stringLiteral ~< DropParser(":") ~ jsonParser
  lazy val objectParser = Literal("{") ~>
    memberParser.manySeparated(",", "object member") ~< "}"
  object UnknownExpression {
    override def toString = "unknown"
  }
  lazy val jsonParser: Self[Any] = DropParser(stringLiteral | objectParser | wholeNumber | arrayParser |
    Fallback(UnknownExpression, "value"))
}
