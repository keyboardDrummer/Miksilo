package transformations.bytecode.types

import core.particles.node.NodeClass
import core.particles.{Delta, Language}

trait StackType extends Delta
{
  val key: NodeClass
  def getStackSize: Int

  override def inject(state: Language): Unit = {
    TypeSkeleton.getRegistry(state).stackSize.put(key, getStackSize)
    super.inject(state)
  }
}
