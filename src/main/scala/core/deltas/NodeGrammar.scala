package core.deltas

import core.bigrammar.BiGrammarToGrammar.WithMap
import core.bigrammar.grammars.MapGrammarWithMap
import core.bigrammar.printer.UndefinedDestructuringValue
import core.bigrammar.{BiGrammar, WithMapG}
import core.deltas.node.{Node, NodeClass, NodeField, NodeLike}

class NodeGrammar(inner: BiGrammar, val key: NodeClass)
  extends MapGrammarWithMap(inner,
    input => NodeGrammar.construct(input, key),
    obj => NodeGrammar.destruct(obj, key))
{
}

object NodeGrammar {

  //noinspection ComparingUnrelatedTypes
  def destruct(withMap: WithMapG[Any], key: NodeClass): Option[WithMapG[Any]] = {
    val value = withMap.value
    if (!value.isInstanceOf[NodeLike])
      return None

    val node = value.asInstanceOf[NodeLike]

    if (node.clazz == key) {
      val dataViewAsGenericMap = node.dataView.map(t => (t._1.asInstanceOf[Any], t._2))
      Some(WithMapG(UndefinedDestructuringValue, dataViewAsGenericMap))
    }
    else {
      None
    }
  }

  case class ValueNotFound(meta: NodeLike, field: Any)

  def construct(withMap: WithMap, key: NodeClass): WithMap = {
    val result = new Node(key)
    result.data ++= withMap.map.collect { case (k: NodeField,v) => (k,v) }
    WithMapG(result, Map.empty)
  }
}