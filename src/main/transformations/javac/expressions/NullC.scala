package transformations.javac.expressions

import core.transformation._
import transformations.bytecode.instructions.PushNullC

import scala.collection.mutable

object NullC extends GrammarTransformation {

  val _null = new MetaObject(NullKey)

  override def transformReserved(reserved: mutable.HashSet[String]): Unit = reserved += "null"

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val expressionGrammar = grammars.find(ExpressionC.ExpressionGrammar)
    val parseNull = "null" ^^ { _ => _null}
    expressionGrammar.inner = expressionGrammar.inner | parseNull
  }

  override def transform(program: MetaObject, state: TransformationState): Unit = {
    ExpressionC.getExpressionToLines(state).put(NullKey, _null => {
      Seq(PushNullC.pushNull)
    })
  }


  override def dependencies: Set[Contract] = Set(ExpressionC, PushNullC)

  object NullKey

}
