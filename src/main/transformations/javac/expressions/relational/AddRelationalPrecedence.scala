package transformations.javac.expressions.relational

import core.particles.grammars.GrammarCatalogue
import core.particles.{Contract, ParticleWithGrammar}
import transformations.javac.expressions.ExpressionSkeleton

object AddRelationalPrecedence extends ParticleWithGrammar {

  override def dependencies: Set[Contract] = Set(ExpressionSkeleton)

  override def transformGrammars(grammars: GrammarCatalogue) = {
    val expressionGrammar = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    val relationalGrammar = grammars.create(RelationalExpressionGrammar, expressionGrammar.inner)
    expressionGrammar.inner = relationalGrammar
  }

  object RelationalExpressionGrammar

  override def description: String = "Creates a named grammar with the correct precedence for relational operators."
}
