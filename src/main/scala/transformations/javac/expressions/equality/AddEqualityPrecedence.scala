package transformations.javac.expressions.equality

import core.particles.grammars.GrammarCatalogue
import core.particles.{Contract, DeltaWithGrammar}
import transformations.javac.expressions.ExpressionSkeleton

object AddEqualityPrecedence extends DeltaWithGrammar {

  override def dependencies: Set[Contract] = Set(ExpressionSkeleton)

  override def transformGrammars(grammars: GrammarCatalogue) = {
    val expressionGrammar = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    val equalityGrammar = grammars.create(EqualityExpressionGrammar, expressionGrammar.inner)
    expressionGrammar.inner = equalityGrammar
  }

  object EqualityExpressionGrammar

  override def description: String = "Creates a named grammar with the correct precedence for equality-like operators."
}
