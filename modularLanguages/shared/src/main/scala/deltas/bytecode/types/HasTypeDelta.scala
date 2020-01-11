package deltas.bytecode.types

import core.deltas.{Delta, HasShape}
import core.language.{Compilation, Language}
import core.language.node.NodeLike
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type

trait HasType {
  def getType(compilation: Compilation, builder: ConstraintBuilder, path: NodeLike, parentScope: Scope) : Type
}

trait HasTypeDelta extends Delta with HasShape with HasType {

  override def inject(language: Language): Unit = {
    super.inject(language)
    TypeSkeleton.hasTypes.add(language, this)
  }
}
