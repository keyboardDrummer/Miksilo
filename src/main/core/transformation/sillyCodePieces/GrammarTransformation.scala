package core.transformation.sillyCodePieces

import core.grammar.~
import core.grammarDocument.GrammarDocumentWriter
import core.transformation.grammars.GrammarCatalogue
import core.transformation.{MetaObject, TransformationState}

trait GrammarTransformation extends Injector with GrammarDocumentWriter {
  def transformGrammars(grammars: GrammarCatalogue)

  override def enter(state: TransformationState): Unit = {
    super.enter(state)
    transformGrammars(state.grammarCatalogue)
  }

  def parseMap(key: AnyRef, fields: Any*): (Any => Any, Any => Option[Any]) = {
    val fieldList = fields.toList
    (input => construct(input, key, fieldList), obj => destruct(obj.asInstanceOf[MetaObject], key, fieldList))
  }

  def destruct(value: MetaObject, key: AnyRef, fields: List[Any]): Option[Any] = {
    if (value.clazz == key) {
      val first :: rest = fields
      var result: Any = value(first)
      for (other <- rest) {
        result = core.grammar.~(result, value(other))
      }
      Some(result)
    } else {
      None
    }
  }

  def tildeValuesToSeq(value: Any) : Seq[Any] = value match {
    case ~(l,r) => tildeValuesToSeq(l) ++ tildeValuesToSeq(r)
    case _ => Seq(value)
  }

  def construct(value: Any, key: AnyRef, fields: List[Any]) = {
    val result = new MetaObject(key)
    val values = tildeValuesToSeq(value)
    fields.zip(values).foreach(pair => result(pair._1) = pair._2)
    result
  }
}
