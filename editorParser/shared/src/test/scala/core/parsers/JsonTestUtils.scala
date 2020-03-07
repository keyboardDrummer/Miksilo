package core.parsers

import languages.json._

object JsonTestUtils {

  def valueToPrimitive(value: JsonValue): Any = {
    value match {
      case NumberLiteral(_, value) => value
      case StringLiteral(_, value) => value
      case JsonArray(_, elements) => elements.map(valueToPrimitive)
      case JsonObject(_, members) => members.map(e => (e._1,valueToPrimitive(e._2)))
      case ValueHole(_) => null
    }
  }
}
