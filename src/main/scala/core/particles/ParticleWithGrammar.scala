package core.particles

import core.bigrammar._
import core.grammar.~
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node, NodeLike}

/*
Used as a field key when mapping a grammar to a node, to indicate that value at this location is mapped not using a regular field key,
but as a map.
 */
object FromMap extends Key

trait ParticleWithGrammar extends Particle with GrammarDocumentWriter {
  implicit val postfixOps = language.postfixOps
  def transformGrammars(grammars: GrammarCatalogue)

  override def inject(state: CompilationState): Unit = {
    super.inject(state)
    transformGrammars(state.grammarCatalogue)
  }

  def parseMapPrimitive(clazz: Class[_]): (Any => Any, Any => Option[Any]) = {
    (x => x, x => if (clazz.isInstance(x)) Some(x) else None)
  }

  case class ValueWasNotAMetaObject(value: Any, clazz: Any) extends RuntimeException
  {
    override def toString = s"value $value was not a MetaObject but used in parseMap for $clazz"
  }

  def parseMap(key: AnyRef, fields: Any*): (Any => Any, Any => Option[Any]) = {
    val fieldList = fields.toList
    (input => oldConstruct(input, key, fieldList), obj => oldDestruct(obj, key, fieldList))
  }

  implicit class GrammarForAst(grammar: BiGrammar)
  {
    def asNode(key: Key, fields: Key*) = new NodeGrammar(grammar, key, fields.toSeq)
    def as(field: Key) = As(grammar, field) //grammar, new NodeMap(grammar, MapInsideNode, fields.toSeq)
  }

  def nodeGrammar(inner: BiGrammar, key: Key, fields: Key*) = new NodeGrammar(inner, key, fields.toSeq)

  class NodeGrammar(inner: BiGrammar, val key: Key, val fields: Seq[Key]) extends MapGrammar(inner, //TODO rename to NodeGrammar?
      input => construct(input.asInstanceOf[WithMap], key, fields.toList),
      obj => destruct(obj.asInstanceOf[WithMap], key, fields.toList), showMap = true)
  {
  }

  //noinspection ComparingUnrelatedTypes
  def destruct(withMap: WithMap, key: Key, fields: List[Key]): Option[WithMap] = {
    val value = withMap.value
    if (!value.isInstanceOf[NodeLike])
      return None

    val node = value.asInstanceOf[NodeLike]

    if (node.clazz == key) {
      val fieldValues = fields.map(field => getFieldValueTakingFromMapIntoAccount(node, field))
      if (fieldValues.isEmpty)
        Some(WithMap(UndefinedDestructuringValue, node.dataView)) //Apparently this node maps onto grammars that are all ignored so it does not contain any values, however we have to return a value here.
      else
        Some(WithMap(fieldValues.reduce((a,b) => core.grammar.~(a,b)), node.dataView))
    } else {
      None
    }
  }

  //noinspection ComparingUnrelatedTypes
  def oldDestruct(value: Any, key: AnyRef, fields: List[Any]): Option[Any] = {
    if (!value.isInstanceOf[NodeLike])
      return None

    val node = value.asInstanceOf[NodeLike]

    if (node.clazz == key) {
      val fieldValues = fields.map(field => getFieldValueTakingFromMapIntoAccount(node, field))
      if (fieldValues.isEmpty)
        Some(UndefinedDestructuringValue) //Apparently this node maps onto grammars that are all ignored so it does not contain any values, however we have to return a value here.
      else
        Some(fieldValues.reduce((a,b) => core.grammar.~(a,b)))
    } else {
      None
    }
  }

  case class ValueNotFound(meta: NodeLike, field: Any)

  def getFieldValueTakingFromMapIntoAccount(meta: NodeLike, key: Any): Any = {
    if (key == FromMap) meta else meta.get(key).getOrElse(ValueNotFound(meta, key))
  }

  def tildeValuesToSeq(value: Any): Seq[Any] = value match {
    case ~(l, r) => tildeValuesToSeq(l) ++ tildeValuesToSeq(r)
    case _ => Seq(value)
  }

  def construct(valueWithMap: WithMap, key: AnyRef, fields: List[Any]) = {
    val value = valueWithMap.value
    val result: Node = oldConstruct(value, key, fields)
    result.data ++= valueWithMap.state.filterKeys(k => k.isInstanceOf[Key])
    WithMap(result, Map.empty)
  }

  def oldConstruct(value: Any, key: AnyRef, fields: List[Any]): Node = {
    val result = new Node(key)
    val values = tildeValuesToSeq(value)
    fields.zip(values).foreach(pair => {
      val field: Any = pair._1
      val fieldValue: Any = pair._2
      if (field == FromMap) {
        fieldValue match {
          case metaFieldValue: Node =>
            result.data ++= fieldValue.asInstanceOf[Node].data
          case _ =>
        }
      }
      else
        result(field) = fieldValue
    })
    result
  }
}
