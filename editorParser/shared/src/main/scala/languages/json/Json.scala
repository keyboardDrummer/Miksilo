package languages.json

import core.parsers.editorParsers.OffsetPointerRange

case class JsonFile(uri: String, value: JsonValue)

trait JsonValue

case class JsonArray(range: OffsetPointerRange, elements: Array[JsonValue]) extends JsonValue
case class JsonObject(range: OffsetPointerRange, members: Array[(String, JsonValue)]) extends JsonValue
case class StringLiteral(range: OffsetPointerRange, value: String) extends JsonValue
case class NumberLiteral(range: OffsetPointerRange, value: Int) extends JsonValue
case class ValueHole(range: OffsetPointerRange) extends JsonValue


