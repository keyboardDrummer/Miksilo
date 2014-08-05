package transformations.javac.expressions

import core.transformation._
import core.transformation.grammars.GrammarCatalogue
import core.transformation.sillyCodePieces.GrammarTransformation
import transformations.bytecode.instructions.PushNullC

object NullC extends GrammarTransformation {

  val _null = new MetaObject(NullKey)

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val expressionGrammar = grammars.find(ExpressionC.ExpressionGrammar)
    val parseNull = "null" ^^ { _ => _null}
    expressionGrammar.inner = expressionGrammar.inner | parseNull
  }

  override def inject(state: TransformationState): Unit = {
    ExpressionC.getExpressionToLines(state).put(NullKey, _null => {
      Seq(PushNullC.pushNull)
    })
  }


  override def dependencies: Set[Contract] = Set(ExpressionC, PushNullC)

  object NullKey

}
