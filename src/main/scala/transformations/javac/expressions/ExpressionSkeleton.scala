package transformations.javac.expressions

import core.particles._
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, NodeWrapper}
import core.particles.path.Path
import transformations.bytecode.types.TypeSkeleton

object ExpressionSkeleton extends DeltaWithGrammar with WithState {

  override def dependencies: Set[Contract] = Set(TypeSkeleton)

  implicit class Expression(val node: Node) extends AnyVal with NodeWrapper

  def getType(state: CompilationState): Path => Node = expression => {
    getState(state).instances(expression.clazz).getType(expression, state)
  }

  def getToInstructions(state: CompilationState): Path => Seq[Node] = {
    expression => getState(state).instances(expression.clazz).toByteCode(expression, state)
  }

  def getToInstructionsRegistry(state: CompilationState) = getState(state).instances

  override def transformGrammars(grammars: GrammarCatalogue, state: CompilationState): Unit =  {
    val core = grammars.create(CoreGrammar)
    grammars.create(ExpressionGrammar, core)
  }

  def createState = new State()
  class State {
    val instances = new ClassRegistry[ExpressionInstance]
  }

  object CoreGrammar
  object ExpressionGrammar

  override def description: String = "Introduces the concept of an expression."
}
