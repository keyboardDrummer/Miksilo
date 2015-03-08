package transformations.javac.methods.assignment

import core.transformation.{ParticleWithGrammar, Contract}
import core.transformation.grammars.GrammarCatalogue
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