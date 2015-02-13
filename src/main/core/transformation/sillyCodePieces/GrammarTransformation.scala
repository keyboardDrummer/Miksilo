package core.transformation.sillyCodePieces

import core.grammar.~
import core.grammarDocument.GrammarDocumentWriter
import core.transformation.grammars.GrammarCatalogue
import core.transformation.{MetaObject, TransformationState}

trait GrammarTransformation extends Particle with GrammarDocumentWriter {
  def transformGrammars(grammars: GrammarCatalogue)

  override def inject(state: TransformationState): Unit = {
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

  object PartialSelf
  def parseMap(key: AnyRef, fields: Any*): (Any => Any, Any => Option[Any]) = {
    val fieldList = fields.toList
    (input => construct(input, key, fieldList), obj => destruct(obj, key, fieldList))
  }

  def destruct(value: Any, key: AnyRef, fields: List[Any]): Option[Any] = {
    if (!value.isInstanceOf[MetaObject])
      return None

    val metaObject = value.asInstanceOf[MetaObject]

    if (metaObject.clazz == key) {
      val first :: rest = fields
      var result: Any = getWithPartial(metaObject, first)
      for (other <- rest) {
        result = core.grammar.~(result, getWithPartial(metaObject, other))
      }
      Some(result)
    } else {
      None
    }
  }

  def getWithPartial(meta: MetaObject, key: Any): Any = {
    if (key == PartialSelf) meta else meta(key)
  }

  def tildeValuesToSeq(value: Any): Seq[Any] = value match {
    case ~(l, r) => tildeValuesToSeq(l) ++ tildeValuesToSeq(r)
    case _ => Seq(value)
  }

  def construct(value: Any, key: AnyRef, fields: List[Any]) = {
    val result = new MetaObject(key)
    val values = tildeValuesToSeq(value)
    fields.zip(values).foreach(pair => {
      val field: Any = pair._1
      val fieldValue: Any = pair._2
      if (field == PartialSelf)
      {
        result.data ++= fieldValue.asInstanceOf[MetaObject].data
      }
      else
        result(field) = fieldValue
    })
    result
  }
}
