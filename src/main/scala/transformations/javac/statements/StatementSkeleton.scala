package transformations.javac.statements

import core.particles._
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, NodeLike, NodeWrapper}
import core.particles.path.Path
import transformations.javac.expressions.ExpressionSkeleton


object StatementSkeleton extends DeltaWithGrammar with WithState {

  implicit class Statement[T <: NodeLike](val node: T) extends NodeWrapper[T] { }

  override def dependencies: Set[Contract] = Set(ExpressionSkeleton)

  def getToInstructions(state: Language): Path => Seq[Node] = {
    statement => getState(state).instances(statement.clazz).toByteCode(statement, state)
  }

  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit =  {
    grammars.create(StatementGrammar)
  }

  object StatementGrammar

  override def description: String = "Defines the concept of a statement."

  class State {
    val instances = new ClassRegistry[StatementInstance]
  }

  override def createState: State = new State()
}
