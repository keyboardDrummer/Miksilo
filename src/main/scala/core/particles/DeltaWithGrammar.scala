package core.particles

import core.bigrammar.BiGrammarToGrammar.WithMap
import core.bigrammar.{MapGrammar, _}
import core.particles.grammars.GrammarCatalogue
import core.particles.node._

trait NodeGrammarWriter extends BiGrammarWriter {

  implicit def grammarAsRoot(grammar: BiGrammar): RootGrammar = new RootGrammar(grammar)
  implicit val postfixOps = language.postfixOps

  case class ValueWasNotAMetaObject(value: Any, clazz: Any) extends RuntimeException
  {
    override def toString = s"value $value was not a MetaObject but used in parseMap for $clazz"
  }

  def parseMapPrimitive(clazz: Class[_]): (Any => Any, Any => Option[Any]) = {
    (x => x, x => Some(x).filter(clazz.isInstance))
  }

  implicit class GrammarForAst(grammar: BiGrammar)
  {
    def parseMap(key: NodeClass): BiGrammar = {
      new MapGrammar(grammar,
        input => construct(input.asInstanceOf[WithMap], key),
        obj => destruct(obj.asInstanceOf[WithMapG[Any]], key), showMap = true)
    }

    def asLabelledNode(grammars: GrammarCatalogue, key: NodeClass): Labelled = grammars.create(key, this.asNode(key))
    def asNode(key: NodeClass) = new NodeGrammar(grammar, key)
    def as(field: NodeField) = As(grammar, field)
  }

  def nodeGrammar(inner: BiGrammar, key: NodeClass) = new NodeGrammar(inner, key)

  class NodeGrammar(inner: BiGrammar, val key: NodeClass)
    extends MapGrammar(inner,
      input => construct(input.asInstanceOf[WithMap], key),
      obj => destruct(obj.asInstanceOf[WithMapG[Any]], key), showMap = true)
  {
  }

  //noinspection ComparingUnrelatedTypes
  def destruct(withMap: WithMapG[Any], key: NodeClass): Option[WithMapG[Any]] = {
    val value = withMap.value
    if (!value.isInstanceOf[NodeLike])
      return None

    val node = value.asInstanceOf[NodeLike]

    if (node.clazz == key) {
      val dataViewAsGenericMap = node.dataView.map(t => (t._1.asInstanceOf[Any], t._2))
      Some(WithMapG(UndefinedDestructuringValue, dataViewAsGenericMap)) //TODO The withMap.state ++ is inconsistent with the construct method. Consistent would be to check that withMap.state is empty.
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

trait DeltaWithGrammar extends Delta with NodeGrammarWriter {

  def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit

  override def inject(state: Language): Unit = {
    super.inject(state)
    transformGrammars(state.grammarCatalogue, state)
  }

}
