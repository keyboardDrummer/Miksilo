package transformations.javac.expressions

import core.particles._
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{Node, NodeWrapper}
import core.particles.path.Path
import transformations.bytecode.types.TypeSkeleton

object ExpressionSkeleton extends DeltaWithGrammar with WithLanguageRegistry {

  override def dependencies: Set[Contract] = Set(TypeSkeleton)

  implicit class Expression(val node: Node) extends NodeWrapper[Node]

  def getType(compilation: Compilation): Path => Node = expression => {
    getRegistry(compilation).instances(expression.clazz).getType(expression, compilation)
  }

  def getToInstructions(compilation: Compilation): Path => Seq[Node] = {
    expression => getRegistry(compilation).instances(expression.clazz).toByteCode(expression, compilation)
  }

  def getToInstructionsRegistry(state: Language) = getRegistry(state).instances

  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit =  {
    val core = grammars.create(CoreGrammar)
    grammars.create(ExpressionGrammar, core)
  }

  def createRegistry = new Registry()
  class Registry {
    val instances = new ClassRegistry[ExpressionInstance]
  }

  object CoreGrammar
  object ExpressionGrammar

  override def description: String = "Introduces the concept of an expression."
}
