package core.parsers

import editorParsers.LeftRecursiveCorrectingParserWriter
import editorParsers.History

object ParseJson extends CommonStringReaderParser with LeftRecursiveCorrectingParserWriter {

  private val nestPenalty = History.missingInputPenalty //+ History.insertFallbackPenalty * 1.1
  lazy val arrayParser = Literal("[", nestPenalty) ~> jsonParser.manySeparated(",", "array element") ~< "]"
  lazy val memberParser = WithDefault(stringLiteral, "") ~< DropParser(":") ~ jsonParser
  lazy val objectParser = Literal("{", nestPenalty) ~>
    memberParser.manySeparated(",", "object member") ~< "}"
  object UnknownExpression {
    override def toString = "unknown"
  }
  lazy val jsonParser: Self[Any] = DropParser(stringLiteral | objectParser | wholeNumber | arrayParser |
    Fallback(UnknownExpression, "value"))
}
