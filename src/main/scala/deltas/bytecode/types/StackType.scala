package deltas.bytecode.types

import core.deltas.node.NodeShape
import core.deltas.Delta
import core.language.Language

trait StackType extends Delta
{
  val key: NodeShape
  def getStackSize: Int

  override def inject(state: Language): Unit = {
    TypeSkeleton.getRegistry(state).stackSize.put(key, getStackSize)
    super.inject(state)
  }
}
