package transformations.bytecode.types

import core.particles.{Language, Delta}
import core.particles.node.Key

trait StackType extends Delta
{
  val key: Key
  def getStackSize: Int

  override def inject(state: Language): Unit = {
    TypeSkeleton.getState(state).stackSize.put(key, getStackSize)
    super.inject(state)
  }
}
