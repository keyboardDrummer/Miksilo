package core.language.node

import core.bigrammar.BiGrammarToParser.WithMap
import core.bigrammar.grammars.{FieldPosition, MapGrammarWithMap}
import core.bigrammar.printer.UndefinedDestructuringValue
import core.bigrammar.{BiGrammar, WithMapG}

class NodeGrammar(inner: BiGrammar, val key: NodeShape)
  extends MapGrammarWithMap(inner,
    input => NodeGrammar.construct(input, key),
    obj => NodeGrammar.destruct(obj, key))
{
}

object NodeGrammar {

  //noinspection ComparingUnrelatedTypes
  def destruct(withMap: WithMapG[Any], shape: NodeShape): Option[WithMapG[Any]] = {
    val value = withMap.value
    if (!value.isInstanceOf[NodeLike])
      return None

    val node = value.asInstanceOf[NodeLike]

    if (node.shape == shape) {
      val dataViewAsGenericMap = node.dataView.map(t => (t._1.asInstanceOf[Any], t._2))
      Some(WithMapG(UndefinedDestructuringValue, dataViewAsGenericMap))
    }
    else {
      None
    }
  }

  case class ValueNotFound(meta: NodeLike, field: Any)

  def construct(withMap: WithMap, key: NodeShape): WithMap = {
    val result = new Node(key)
    result.data ++= withMap.map.collect { case (k: NodeField,v) => (k,v) }
    result.sources ++= withMap.map.collect { case (k: FieldPosition,v) => (k.field,v.asInstanceOf[SourceRange]) }
    WithMapG(result, Map.empty)
  }
}