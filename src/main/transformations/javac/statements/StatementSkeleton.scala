package transformations.javac.statements

import core.particles._
import core.particles.grammars.GrammarCatalogue
import transformations.javac.expressions.ExpressionSkeleton
import transformations.javac.expressions.ExpressionSkeleton.MissingToInstructionsFor

import scala.collection.mutable


object StatementSkeleton extends ParticleWithGrammar with WithState {

  override def dependencies: Set[Contract] = Set(ExpressionSkeleton)

  def getToInstructions(state: CompilationState): Path => Seq[MetaObject] = {
    statement => {
      val statementTransformation = getState(state).instances.get(statement.clazz)
      val transformation = statementTransformation.getOrElse(throw new MissingToInstructionsFor(statement.clazz))
      transformation.toByteCode(statement, state)
    }
  }

  override def transformGrammars(grammars: GrammarCatalogue) {
    grammars.create(StatementGrammar)
  }

  object StatementGrammar

  override def description: String = "Defines the concept of a statement."

  class State {
    val instances = new ClassRegistry[StatementInstance]
  }

  override def createState: State = new State()
}
