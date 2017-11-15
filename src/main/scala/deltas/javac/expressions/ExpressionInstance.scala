package deltas.javac.expressions

import core.particles._
import core.particles.node.{Node, NodeClass}
import core.particles.path.Path

trait ExpressionInstance extends DeltaWithGrammar {
  val key: NodeClass

  override def inject(state: Language): Unit = {
    ExpressionSkeleton.getRegistry(state).instances.put(key, this)
    super.inject(state)
  }

  def toByteCode(expression: Path, compilation: Compilation): Seq[Node]

  /**
   * Given expression is a path so that a variablePool can be retrieved.
   */
  def getType(expression: Path, compilation: Compilation): Node

  override def dependencies: Set[Contract] = Set(ExpressionSkeleton)
}
