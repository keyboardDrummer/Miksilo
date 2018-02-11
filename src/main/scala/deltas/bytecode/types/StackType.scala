package deltas.bytecode.types

import core.deltas.node.NodeShape
import core.deltas.Delta
import core.language.Language

trait StackType extends Delta //TODO should this be merged with ByteCodeTypeInstance?
{
  val shape: NodeShape
  def getStackSize: Int

  override def inject(language: Language): Unit = {
    TypeSkeleton.getRegistry(language).stackSize.put(shape, getStackSize)
    super.inject(language)
  }
}
