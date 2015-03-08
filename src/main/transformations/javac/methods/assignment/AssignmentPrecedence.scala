package transformations.javac.methods.assignment

import core.particles.{ParticleWithGrammar, Contract}
import core.particles.grammars.GrammarCatalogue
import transformations.javac.expressions.ExpressionSkeleton

object AssignmentPrecedence extends ParticleWithGrammar {

  override def dependencies: Set[Contract] = Set(ExpressionSkeleton)

  override def transformGrammars(grammars: GrammarCatalogue) = {
    val expressionGrammar = grammars.find(ExpressionSkeleton.ExpressionGrammar)
    val assignmentGrammar = grammars.create(AssignmentGrammar, expressionGrammar.inner)
    expressionGrammar.inner = assignmentGrammar
  }

  object AssignmentGrammar

  override def description: String = "Wraps the current expression grammar in a failing assignment grammar."
}