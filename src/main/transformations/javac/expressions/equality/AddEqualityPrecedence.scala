package transformations.javac.expressions.equality

import core.transformation.Contract
import core.transformation.grammars.GrammarCatalogue
import core.transformation.sillyCodePieces.GrammarTransformation
import transformations.javac.expressions.ExpressionSkeleton

object AddEqualityPrecedence extends GrammarTransformation {

  override def dependencies: Set[Contract] = Set(ExpressionSkeleton)

  override def transformGrammars(grammars: GrammarCatalogue) = {
    val expressionGrammar = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    val equalityGrammar = grammars.create(EqualityExpressionGrammar, expressionGrammar.inner)
    expressionGrammar.inner = equalityGrammar
  }

  object EqualityExpressionGrammar

  override def description: String = "Creates a named grammar with the correct precedence for equality-like operators."
}
