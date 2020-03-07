package languages.json

import core.parsers.editorParsers.OffsetNodeRange

trait JsonValue

case class JsonArray(range: OffsetNodeRange, elements: Array[JsonValue]) extends JsonValue
case class JsonObject(range: OffsetNodeRange, members: Array[(String, JsonValue)]) extends JsonValue
case class StringLiteral(range: OffsetNodeRange, value: String) extends JsonValue
case class NumberLiteral(range: OffsetNodeRange, value: Int) extends JsonValue
case class ValueHole(range: OffsetNodeRange) extends JsonValue


