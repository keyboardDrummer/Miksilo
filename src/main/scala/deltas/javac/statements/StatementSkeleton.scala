package deltas.javac.statements

import core.deltas._
import core.deltas.grammars.LanguageGrammars
import core.deltas.node.{GrammarKey, Node, NodeLike, NodeWrapper}
import core.deltas.path.NodePath
import core.language.Language
import deltas.javac.expressions.ExpressionSkeleton


object StatementSkeleton extends DeltaWithGrammar with WithLanguageRegistry {

  implicit class Statement[T <: NodeLike](val node: T) extends NodeWrapper[T] { }

  override def dependencies: Set[Contract] = Set(ExpressionSkeleton)

  def getToInstructions(compilation: Compilation): NodePath => Seq[Node] = {
    statement => getRegistry(compilation).instances(statement.shape).toByteCode(statement, compilation)
  }

  override def transformGrammars(grammars: LanguageGrammars, state: Language): Unit =  {
    grammars.create(StatementGrammar)
  }

  object StatementGrammar extends GrammarKey

  override def description: String = "Defines the concept of a statement."

  class Registry {
    val instances = new ShapeRegistry[StatementInstance]
  }

  override def createRegistry: Registry = new Registry()
}
