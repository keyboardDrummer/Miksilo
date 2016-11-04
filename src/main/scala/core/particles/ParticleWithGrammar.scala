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

/*
Used for the moment because we can't yet store parsed values into some map.
We put them into a Node because it already has support for tupling/detupling
 */
object MapInsideNode extends Key

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
    (input => construct(input, key, fieldList), obj => destruct(obj, key, fieldList))
  }

  implicit class LabelledGrammarForAst(grammar: Labelled)
  {
    /* This is a somewhat hacky way to remove part of a grammar that was used by a node.
    It requires that this grammar part was included using .as and that the Node was using PartialSelf
    We produce an UndefinedDestructuringValue because that's also what you get when you 'ignore' part of a grammar.
     */
    def remove() : Unit = grammar.inner = produce(UndefinedDestructuringValue).as()
  }

  implicit class GrammarForAst(grammar: BiGrammar)
  {
    def asNode(key: Key, fields: Key*) = new NodeMap(grammar, key, fields.toSeq)
    def as(fields: Key*) = new NodeMap(grammar, MapInsideNode, fields.toSeq)
  }

  def nodeMap(inner: BiGrammar, key: Key, fields: Key*) = new NodeMap(inner, key, fields.toSeq)

  class NodeMap(inner: BiGrammar, val key: Key, val fields: Seq[Key]) extends MapGrammar(inner,
      input => construct(input, key, fields.toList),
      obj => destruct(obj, key, fields.toList))
  {
  }

  //noinspection ComparingUnrelatedTypes
  def destruct(value: Any, key: AnyRef, fields: List[Any]): Option[Any] = {
    if (!value.isInstanceOf[NodeLike])
      return None

    val node = value.asInstanceOf[NodeLike]

    val ignoreNodeClazz: Boolean = key == MapInsideNode //When we're hiding a map in a node we don't care about the node's clazz.
    if (node.clazz == key || ignoreNodeClazz) {
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

  def construct(value: Any, key: AnyRef, fields: List[Any]) = {
    val result = new Node(key)
    val values = tildeValuesToSeq(value)
    fields.zip(values).foreach(pair => {
      val field: Any = pair._1
      val fieldValue: Any = pair._2
      if (field == FromMap)
      {
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
