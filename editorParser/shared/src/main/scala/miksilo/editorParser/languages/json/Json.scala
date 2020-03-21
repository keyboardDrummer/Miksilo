package miksilo.editorParser.languages.json

import miksilo.editorParser.parsers.SourceElement
import miksilo.editorParser.parsers.editorParsers.OffsetPointerRange

import scala.collection.immutable.ListMap

trait JsonValue extends SourceElement

case class JsonArray(rangeOption: Option[OffsetPointerRange], elements: Array[JsonValue]) extends JsonValue
case class JsonObject(rangeOption: Option[OffsetPointerRange], members: ListMap[String, JsonValue]) extends JsonValue {
}
case class StringLiteral(rangeOption: Option[OffsetPointerRange], value: String) extends JsonValue
case class NumberLiteral(rangeOption: Option[OffsetPointerRange], value: Int) extends JsonValue
case class ValueHole(rangeOption: Option[OffsetPointerRange]) extends JsonValue


