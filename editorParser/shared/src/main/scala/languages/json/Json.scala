package languages.json

import core.parsers.SourceElement
import core.parsers.editorParsers.OffsetPointerRange

trait JsonValue extends SourceElement

case class JsonArray(rangeOption: Option[OffsetPointerRange], elements: Array[JsonValue]) extends JsonValue
case class JsonObject(rangeOption: Option[OffsetPointerRange], members: Array[(String, JsonValue)]) extends JsonValue
case class StringLiteral(rangeOption: Option[OffsetPointerRange], value: String) extends JsonValue
case class NumberLiteral(rangeOption: Option[OffsetPointerRange], value: Int) extends JsonValue
case class ValueHole(rangeOption: Option[OffsetPointerRange]) extends JsonValue


