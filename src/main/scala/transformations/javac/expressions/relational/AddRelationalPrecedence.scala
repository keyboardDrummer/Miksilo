package transformations.javac.expressions.relational

import core.particles.grammars.GrammarCatalogue
import core.particles.node.GrammarKey
import core.particles.{Contract, DeltaWithGrammar, Language}
import transformations.javac.expressions.ExpressionSkeleton

object AddRelationalPrecedence extends DeltaWithGrammar {

  override def dependencies: Set[Contract] = Set(ExpressionSkeleton)

  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    val expressionGrammar = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    val relationalGrammar = grammars.create(RelationalExpressionGrammar, expressionGrammar.inner)
    expressionGrammar.inner = relationalGrammar
  }

  object RelationalExpressionGrammar extends GrammarKey

  override def description: String = "Creates a named grammar with the correct precedence for relational operators."
}
