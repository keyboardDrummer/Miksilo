package transformations.javac.statements

import core.particles._
import core.particles.grammars.GrammarCatalogue
import core.particles.node.Node
import core.particles.path.Path
import transformations.javac.expressions.ExpressionSkeleton

object StatementSkeleton extends ParticleWithGrammar with WithState {

  override def dependencies: Set[Contract] = Set(ExpressionSkeleton)

  def getToInstructions(state: CompilationState): Path => Seq[Node] = {
    statement => getState(state).instances(statement.clazz).toByteCode(statement, state)
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
