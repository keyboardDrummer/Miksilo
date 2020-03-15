package miksilo.modularLanguages.deltas.classes

import miksilo.modularLanguages.core.deltas.{Delta, HasShape}
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.modularLanguages.deltas.ConstraintSkeleton

trait HasConstraintsDelta extends Delta with HasShape with HasConstraints {

  override def inject(language: Language): Unit = {
    super.inject(language)
    ConstraintSkeleton.hasConstraints.add(language, this)
  }
}

trait HasConstraints {
  def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope) : Unit
}
