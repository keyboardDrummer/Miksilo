package core.language.node

import core.bigrammar.BiGrammarToParser.AnyWithMap
import core.bigrammar.grammars.{FieldPosition, MapGrammar}
import core.bigrammar.{BiGrammar, WithMap}
import languageServer.SourceRange

class NodeGrammar(inner: BiGrammar[WithMap[Unit]], val shape: NodeShape)
  extends MapGrammar[WithMap[Unit], Node](inner,
    input => Right(NodeGrammar.construct(input, shape)),
    obj => NodeGrammar.destruct(obj, shape))
{
  override def withChildren(newChildren: Seq[BiGrammar[_]]): NodeGrammar =
    new NodeGrammar(newChildren.head.asInstanceOf[BiGrammar[WithMap[Unit]]], shape)
}

object NodeGrammar {

  //noinspection ComparingUnrelatedTypes
  def destruct(node: Node, shape: NodeShape): Option[WithMap[Unit]] = {
    if (node.shape == shape) {
      val dataViewAsGenericMap = node.dataView.map(t => (t._1.asInstanceOf[Any], t._2))
      Some(WithMap(Unit, dataViewAsGenericMap))
    }
    else {
      None
    }
  }

  case class ValueNotFound(meta: NodeLike, field: Any)

  def construct(withMap: AnyWithMap, key: NodeShape): Node = {
    val result = new Node(key)
    result.data ++= withMap.namedValues.collect { case (k: NodeField,v) => (k,v) } // TODO trivia should be stored in a separate array on Node.
    result.sources ++= withMap.namedValues.collect { case (k: FieldPosition,v) => (k.field,v.asInstanceOf[SourceRange]) }
    result
  }
}