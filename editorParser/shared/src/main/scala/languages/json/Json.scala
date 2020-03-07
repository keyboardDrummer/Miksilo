package languages.json

import core.parsers.core.ParseText
import core.parsers.editorParsers.{History, LeftRecursiveCorrectingParserWriter, OffsetNodeRange}
import core.parsers.strings.{CommonStringReaderParser, WhitespaceParserWriter}

trait JsonValue

case class JsonArray(range: OffsetNodeRange, elements: Array[JsonValue]) extends JsonValue
case class JsonObject(range: OffsetNodeRange, members: Array[(String, JsonValue)]) extends JsonValue
case class StringLiteral(range: OffsetNodeRange, value: String) extends JsonValue
case class NumberLiteral(range: OffsetNodeRange, value: Int) extends JsonValue
case class ValueHole(range: OffsetNodeRange) extends JsonValue


