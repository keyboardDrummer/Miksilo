package deltas.javac.classes.skeleton

import core.deltas.{Delta, HasShape}
import core.deltas.path.NodePath
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope

trait HasConstraints extends Delta with HasShape {
  def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope) : Unit

  override def inject(language: Language): Unit = {
    super.inject(language)
    JavaClassSkeleton.hasConstraints.add(language, this)
  }
}
