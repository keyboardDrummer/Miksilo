package languages.json

import core.parsers.editorParsers.{LeftRecursiveCorrectingParserWriter, OffsetNodeRange}
import core.parsers.strings.{CommonStringReaderParser, WhitespaceParserWriter}

trait JsonValue

case class JsonArray(range: OffsetNodeRange, elements: Array[JsonValue]) extends JsonValue
case class JsonObject(range: OffsetNodeRange, members: Map[String, JsonValue]) extends JsonValue
case class StringLiteral(range: OffsetNodeRange, value: String) extends JsonValue
case class NumberLiteral(range: OffsetNodeRange, value: String) extends JsonValue
case class ValueHole(range: OffsetNodeRange) extends JsonValue

object JsonParser extends CommonStringReaderParser with LeftRecursiveCorrectingParserWriter with WhitespaceParserWriter  {
  val parser = valueParser.getWholeInputParser()
  lazy val valueParser: Parser[JsonValue] = new Lazy(array | objectParser | number | string | hole)

  val array = ("[" ~> valueParser.manySeparated(",", "value") ~< "]").
    withSourceRange((range, value) => JsonArray(range, value.toArray))
  val objectMember = stringLiteral ~< ":" ~ valueParser
  val objectParser = ("{" ~> objectMember.manySeparated(",", "member") ~< "}").
    withSourceRange((range, value) => JsonObject(range, value.toMap))
  val number = wholeNumber.withSourceRange((range, value) => NumberLiteral(range, value))
  val string = stringLiteral.withSourceRange((range, value) => StringLiteral(range, value))
  val hole = Fallback(RegexParser(" *".r, "spaces").withSourceRange((range,_) => ValueHole(range)), "value")
}


