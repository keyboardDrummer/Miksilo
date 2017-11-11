package transformations.javac.statements

import core.particles._
import core.particles.grammars.LanguageGrammars
import core.particles.node.{GrammarKey, Node, NodeLike, NodeWrapper}
import core.particles.path.Path
import transformations.javac.expressions.ExpressionSkeleton


object StatementSkeleton extends DeltaWithGrammar with WithLanguageRegistry {

  implicit class Statement[T <: NodeLike](val node: T) extends NodeWrapper[T] { }

  override def dependencies: Set[Contract] = Set(ExpressionSkeleton)

  def getToInstructions(compilation: Compilation): Path => Seq[Node] = {
    statement => getRegistry(compilation).instances(statement.clazz).toByteCode(statement, compilation)
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit =  {
    grammars.create(StatementGrammar)
  }

  object StatementGrammar extends GrammarKey

  override def description: String = "Defines the concept of a statement."

  class Registry {
    val instances = new ClassRegistry[StatementInstance]
  }

  override def createRegistry: Registry = new Registry()
}
