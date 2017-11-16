package deltas.bytecode.types

import core.deltas.node.NodeClass
import core.deltas.{Delta, Language}

trait StackType extends Delta
{
  val key: NodeClass
  def getStackSize: Int

  override def inject(state: Language): Unit = {
    TypeSkeleton.getRegistry(state).stackSize.put(key, getStackSize)
    super.inject(state)
  }
}
