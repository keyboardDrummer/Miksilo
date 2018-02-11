package deltas.bytecode.types

import core.deltas.node.NodeLike
import core.deltas.{Compilation, Delta, HasShape}
import core.language.Language
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type

trait HasType extends Delta with HasShape {

  override def inject(language: Language): Unit = {
    super.inject(language)
    TypeSkeleton.hasTypes.add(language, this)
  }

  def getType(compilation: Compilation, builder: ConstraintBuilder, path: NodeLike, parentScope: Scope) : Type
}
