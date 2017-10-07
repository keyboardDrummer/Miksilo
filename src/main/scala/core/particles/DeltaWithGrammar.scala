package core.particles

import core.bigrammar.{MapGrammar, _}
import core.grammar.~
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
    def parseMap(key: NodeClass, fields: NodeField*): BiGrammar = {
      new MapGrammar(grammar,
        input => construct(input.asInstanceOf[WithMap], key, fields.toList),
        obj => destruct(obj.asInstanceOf[WithMap], key, fields.toList), showMap = true)
    }

    def asLabelledNode(grammars: GrammarCatalogue, key: NodeClass): Labelled = grammars.create(key, this.asNode(key))
    def asNode(key: NodeClass, fields: NodeField*) = new NodeGrammar(grammar, key, fields.toSeq)
    def as(field: NodeField) = As(grammar, field)
  }

  def nodeGrammar(inner: BiGrammar, key: NodeClass, fields: NodeField*) = new NodeGrammar(inner, key, fields.toSeq)

  class NodeGrammar(inner: BiGrammar, val key: NodeClass, val fields: Seq[NodeField])
    extends MapGrammar(inner,
      input => construct(input.asInstanceOf[WithMap], key, fields.toList),
      obj => destruct(obj.asInstanceOf[WithMap], key, fields.toList), showMap = true)
  {
  }

  //noinspection ComparingUnrelatedTypes
  def destruct(withMap: WithMap, key: NodeClass, fields: List[NodeField]): Option[WithMap] = {
    val value = withMap.value
    if (!value.isInstanceOf[NodeLike])
      return None

    val node = value.asInstanceOf[NodeLike]

    if (node.clazz == key) {
      val fieldValues = fields.map(field => node.get(field).getOrElse(ValueNotFound(node, field)))
      val dataViewAsGenericMap = node.dataView.map(t => (t._1.asInstanceOf[Any], t._2))
      if (fieldValues.isEmpty) {
        Some(WithMap(UndefinedDestructuringValue, dataViewAsGenericMap))
      } //Apparently this node maps onto grammars that are all ignored so it does not contain any values, however we have to return a value here.
      else
        Some(WithMap(fieldValues.reduce((a,b) => core.grammar.~(a,b)), dataViewAsGenericMap))
    } else {
      None
    }
  }

  case class ValueNotFound(meta: NodeLike, field: Any)

  def tildeValuesToSeq(value: Any): Seq[Any] = value match {
    case ~(l, r) => tildeValuesToSeq(l) ++ tildeValuesToSeq(r)
    case _ => Seq(value)
  }

  def construct(valueWithMap: WithMap, key: NodeClass, fields: List[NodeField]): WithMap = {
    val value = valueWithMap.value

    val result = new Node(key)
    val values = tildeValuesToSeq(value)
    fields.zip(values).foreach(pair => {
      val field = pair._1
      val fieldValue: Any = pair._2
      result(field) = fieldValue
    })
    result.data ++= valueWithMap.state.collect { case (k: NodeField,v) => (k,v) }
    WithMap(result, Map.empty)
  }
}

trait DeltaWithGrammar extends Delta with NodeGrammarWriter {

  def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit

  override def inject(state: Language): Unit = {
    super.inject(state)
    transformGrammars(state.grammarCatalogue, state)
  }

}
