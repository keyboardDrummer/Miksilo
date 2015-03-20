package transformations.javac.expressions

import core.particles._
import core.particles.node.Node
import core.particles.path.Path

trait ExpressionInstance extends ParticleWithGrammar {
  val key: AnyRef

  override def inject(state: CompilationState): Unit = {
    ExpressionSkeleton.getState(state).instances.put(key, this)
    super.inject(state)
  }

  def toByteCode(expression: Path, state: CompilationState): Seq[Node]

  def getType(expression: Path, state: CompilationState): Node


  override def dependencies: Set[Contract] = Set(ExpressionSkeleton)
}
