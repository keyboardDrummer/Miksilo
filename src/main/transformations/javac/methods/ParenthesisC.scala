package transformations.javac.methods

import core.transformation.{Contract, GrammarCatalogue, GrammarTransformation}
import transformations.javac.expressions.ExpressionC

object ParenthesisC extends GrammarTransformation {

  override def dependencies: Set[Contract] = Set(ExpressionC)

  override def transformGrammars(grammars: GrammarCatalogue): Unit = {
    val expression = grammars.find(ExpressionC.ExpressionGrammar)
    val parseParenthesis = "(" ~> expression <~ ")"
    expression.inner = expression.inner | parseParenthesis
  }
}
