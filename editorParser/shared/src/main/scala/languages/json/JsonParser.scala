package languages.json

import core.parsers.core.ParseText
import core.parsers.editorParsers.{History, LeftRecursiveCorrectingParserWriter}
import core.parsers.strings.{CommonStringReaderParser, WhitespaceParserWriter}

object JsonParser extends CommonStringReaderParser with LeftRecursiveCorrectingParserWriter with WhitespaceParserWriter  {

  lazy val array = ("[" ~> valueParser.manySeparated(",", "value") ~< "]").
    withSourceRange((range, value) => JsonArray(range, value.toArray))
  lazy val objectMember = stringLiteral ~< ":" ~ valueParser
  lazy val objectParser = (literal("{", 2 * History.missingInputPenalty) ~> objectMember.manySeparated(",", "member") ~< "}").
    withSourceRange((range, value) => JsonObject(range, value.toArray))
  lazy val number = wholeNumber.withSourceRange((range, value) => NumberLiteral(range, Integer.parseInt(value)))
  lazy val string = stringLiteral.withSourceRange((range, value) => StringLiteral(range, value))
  lazy val hole = Fallback(RegexParser(" *".r, "spaces").withSourceRange((range,_) => ValueHole(range)), "value")

  lazy val valueParser: Parser[JsonValue] = new Lazy(array | objectParser | number | string | hole)
  def getParser(text: ParseText = new ParseText()) = valueParser.getWholeInputParser(text)
}
