package miksilo.editorParser.parsers

import miksilo.editorParser.languages.json._

import scala.collection.immutable.ListMap

object JsonTestUtils {

  def valueToPrimitive(value: JsonValue): Any = {
    value match {
      case NumberLiteral(_, value) => value
      case StringLiteral(_, value) => value
      case JsonArray(_, elements) => elements.toList.map(valueToPrimitive)
      case JsonObject(_, members) => ListMap.from(members.map(e => (e._1,valueToPrimitive(e._2))))
      case ValueHole(_) => null
    }
  }
}
