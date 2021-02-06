package miksilo.editorParser.parsers

import miksilo.editorParser.languages.yaml.{NumberLiteral, StringLiteral, TaggedNode, ValueHole, YamlArray, YamlObject, YamlValue}

import scala.collection.immutable.ListMap

object YamlTestUtils {

  def valueToPrimitive(value: YamlValue): Any = {
    value match {
      case NumberLiteral(_, value) => value
      case StringLiteral(_, value) => value
      case YamlArray(_, elements) => elements.toList.map(valueToPrimitive)
      case YamlObject(_, members) => ListMap.from(members.map(e => (valueToPrimitive(e._1), valueToPrimitive(e._2))))
      case TaggedNode(rangeOption, tag, node) => (tag, valueToPrimitive(node))
      case ValueHole(_) => null
    }
  }
}
