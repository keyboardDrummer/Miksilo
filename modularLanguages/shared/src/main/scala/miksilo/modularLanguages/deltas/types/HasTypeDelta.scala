package miksilo.modularLanguages.deltas.bytecode.types

import miksilo.modularLanguages.core.deltas.{Delta, HasShape}
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.modularLanguages.core.node.NodeLike
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.languageServer.core.smarts.types.objects.Type

trait HasType {
  def getType(compilation: Compilation, builder: ConstraintBuilder, path: NodeLike, parentScope: Scope) : Type
}

trait HasTypeDelta extends Delta with HasShape with HasType {

  override def inject(language: Language): Unit = {
    super.inject(language)
    TypeSkeleton.hasTypes.add(language, this)
  }
}
