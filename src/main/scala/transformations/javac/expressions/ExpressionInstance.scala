package transformations.javac.expressions

import core.particles._
import core.particles.node.Node
import core.particles.path.Path

trait ExpressionInstance extends DeltaWithGrammar {
  val key: AnyRef

  override def inject(state: Language): Unit = {
    ExpressionSkeleton.getState(state).instances.put(key, this)
    super.inject(state)
  }

  def toByteCode(expression: Path, state: Language): Seq[Node]

  /**
   * Given expression is a path so that a variablePool can be retrieved.
   */
  def getType(expression: Path, state: Language): Node

  override def dependencies: Set[Contract] = Set(ExpressionSkeleton)
}
