package core.parsers

import editorParsers.LeftRecursiveCorrectingParserWriter

object ParseJson extends CommonStringReaderParser with LeftRecursiveCorrectingParserWriter {

  lazy val arrayParser = "[" ~> jsonParser.manySeparated(",", "array element") ~< "]"
  lazy val memberParser = stringLiteral ~< DropParser(":") ~ jsonParser
  lazy val objectParser = "{" ~> memberParser.manySeparated(",", "object member") ~< "}"
  object UnknownExpression {
    override def toString = "unknown"
  }
  lazy val jsonParser: Self[Any] = DropParser((stringLiteral | objectParser | wholeNumber | arrayParser).
    withDefault(UnknownExpression, "value"))
}
