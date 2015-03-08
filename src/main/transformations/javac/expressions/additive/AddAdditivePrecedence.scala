package transformations.javac.expressions.additive

import core.transformation.Contract
import core.transformation.grammars.GrammarCatalogue
import core.transformation.sillyCodePieces.GrammarTransformation
import transformations.javac.expressions.ExpressionSkeleton

object AddAdditivePrecedence extends GrammarTransformation {

  override def dependencies: Set[Contract] = Set(ExpressionSkeleton)

  override def transformGrammars(grammars: GrammarCatalogue) = {
    val expressionGrammar = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    val additiveGrammar = grammars.create(AdditiveExpressionGrammar, expressionGrammar.inner)
    expressionGrammar.inner = additiveGrammar
  }

  object AdditiveExpressionGrammar

  override def description: String = "Creates a named grammar with the correct precedence for addition-like operators."
}
