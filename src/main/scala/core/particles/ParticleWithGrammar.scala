package core.particles

import core.bigrammar.{VoidValue, BiGrammar, MapGrammar, GrammarDocumentWriter}
import core.grammar.~
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Key, Node, NodeLike}

object PartialSelf extends Key

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

  implicit class GrammarForAst(grammar: BiGrammar)
  {
    def asNode(key: Key, fields: Key*) = new NodeMap(grammar, key, fields.toSeq)
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

    val metaObject = value.asInstanceOf[NodeLike]

    if (metaObject.clazz == key || key == PartialSelf) {
      val fieldValues = fields.map(field => getWithPartial(metaObject, field))
      if (fieldValues.isEmpty)
        Some(VoidValue)
      else
        Some(fieldValues.reduce((a,b) => core.grammar.~(a,b)))
    } else {
      None
    }
  }

  case class ValueNotFound(meta: NodeLike, field: Any)

  def getWithPartial(meta: NodeLike, key: Any): Any = {
    if (key == PartialSelf) meta else meta.get(key).getOrElse(ValueNotFound(meta, key))
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
      if (field == PartialSelf)
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
