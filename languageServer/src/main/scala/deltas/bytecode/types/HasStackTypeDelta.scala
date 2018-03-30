package deltas.bytecode.types

import core.deltas.{Delta, HasShape}
import core.language.Language

trait HasStackTypeDelta extends Delta with HasShape //TODO should this be merged with ByteCodeTypeInstance?
{
  def getStackSize: Int

  override def inject(language: Language): Unit = {
    TypeSkeleton.hasStackSize.add(language, shape, getStackSize)
    super.inject(language)
  }
}
