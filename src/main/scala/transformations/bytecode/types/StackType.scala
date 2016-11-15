package transformations.bytecode.types

import core.particles.{CompilationState, Delta}
import core.particles.node.Key

trait StackType extends Delta
{
  val key: Key
  def getStackSize: Int

  override def inject(state: CompilationState): Unit = {
    TypeSkeleton.getState(state).stackSize.put(key, getStackSize)
    super.inject(state)
  }
}
