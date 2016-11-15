package transformations.javac.expressions.additive

import core.particles.grammars.GrammarCatalogue
import core.particles.{Contract, DeltaWithGrammar}
import transformations.javac.expressions.ExpressionSkeleton

object AddAdditivePrecedence extends DeltaWithGrammar {

  override def dependencies: Set[Contract] = Set(ExpressionSkeleton)

  override def transformGrammars(grammars: GrammarCatalogue) = {
    val expressionGrammar = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    val additiveGrammar = grammars.create(AdditiveExpressionGrammar, expressionGrammar.inner)
    expressionGrammar.inner = additiveGrammar
  }

  object AdditiveExpressionGrammar

  override def description: String = "Creates a named grammar with the correct precedence for addition-like operators."
}
