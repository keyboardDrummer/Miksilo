package deltas.javac.expressions

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{GrammarKey, Key, Node, NodeWrapper}
import core.deltas.path.Path
import deltas.bytecode.types.TypeSkeleton

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

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit =  {
    val core = grammars.create(CoreGrammar)
    grammars.create(ExpressionGrammar, core)
  }

  def createRegistry = new Registry()
  class Registry {
    val instances = new ClassRegistry[ExpressionInstance]
  }

  object CoreGrammar extends GrammarKey
  object ExpressionGrammar extends GrammarKey

  override def description: String = "Introduces the concept of an expression."
}
