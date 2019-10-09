package deltas.javac.classes.skeleton

import core.deltas.{Delta, HasShape}
import core.deltas.path.NodePath
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import deltas.ConstraintSkeleton

trait HasConstraintsDelta extends Delta with HasShape with HasConstraints {

  override def inject(language: Language): Unit = {
    super.inject(language)
    ConstraintSkeleton.hasConstraints.add(language, this)
  }
}

trait HasConstraints {
  def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope) : Unit
}
