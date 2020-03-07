package languages.json

import core.parsers.core.ParseText
import core.parsers.editorParsers.{LeftRecursiveCorrectingParserWriter, OffsetNodeRange}
import core.parsers.strings.{CommonStringReaderParser, WhitespaceParserWriter}

trait JsonValue

case class JsonArray(range: OffsetNodeRange, elements: Array[JsonValue]) extends JsonValue
case class JsonObject(range: OffsetNodeRange, members: Array[(String, JsonValue)]) extends JsonValue
case class StringLiteral(range: OffsetNodeRange, value: String) extends JsonValue
case class NumberLiteral(range: OffsetNodeRange, value: Int) extends JsonValue
case class ValueHole(range: OffsetNodeRange) extends JsonValue

object JsonParser extends CommonStringReaderParser with LeftRecursiveCorrectingParserWriter with WhitespaceParserWriter  {

  lazy val array = ("[" ~> valueParser.manySeparated(",", "value") ~< "]").
    withSourceRange((range, value) => JsonArray(range, value.toArray))
  lazy val objectMember = stringLiteral ~< ":" ~ valueParser
  lazy val objectParser = ("{" ~> objectMember.manySeparated(",", "member") ~< "}").
    withSourceRange((range, value) => JsonObject(range, value.toArray))
  lazy val number = wholeNumber.withSourceRange((range, value) => NumberLiteral(range, Integer.parseInt(value)))
  lazy val string = stringLiteral.withSourceRange((range, value) => StringLiteral(range, value))
  lazy val hole = Fallback(RegexParser(" *".r, "spaces").withSourceRange((range,_) => ValueHole(range)), "value")

  lazy val valueParser: Parser[JsonValue] = new Lazy(array | objectParser | number | string | hole)
  def getParser(text: ParseText = new ParseText()) = valueParser.getWholeInputParser(text)
}


