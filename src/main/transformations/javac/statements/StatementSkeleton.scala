package transformations.javac.statements

import core.particles._
import core.particles.grammars.GrammarCatalogue
import transformations.javac.expressions.ExpressionSkeleton
import transformations.javac.expressions.ExpressionSkeleton.MissingToInstructionsFor

import scala.collection.mutable


object StatementSkeleton extends ParticleWithGrammar {

  override def dependencies: Set[Contract] = Set(ExpressionSkeleton)

  def getToInstructions(state: CompilationState): MetaObject => Seq[MetaObject] = {
    statement => {
      val statementTransformation = getStatementToLines(state).get(statement.clazz)
      val transformation = statementTransformation.getOrElse(throw new MissingToInstructionsFor(statement.clazz))
      transformation(statement)
    }
  }

  def getStatementToLines(state: CompilationState): StatementTransformations =
    state.data.getOrElseUpdate(this, new StatementTransformations()).asInstanceOf[StatementTransformations]

  override def transformGrammars(grammars: GrammarCatalogue) {
    grammars.create(StatementGrammar)
  }

  class StatementTransformations extends mutable.HashMap[AnyRef, MetaObject => Seq[MetaObject]]

  object StatementGrammar

  override def description: String = "Defines the concept of a statement."
}
