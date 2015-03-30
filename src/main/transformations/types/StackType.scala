package transformations.types

import core.particles.{CompilationState, Particle}
import core.particles.node.Key

trait StackType extends Particle
{
  val key: Key
  def getStackSize: Int

  override def inject(state: CompilationState): Unit = {
    TypeSkeleton.getState(state).stackSize.put(key, getStackSize)
    super.inject(state)
  }
}
