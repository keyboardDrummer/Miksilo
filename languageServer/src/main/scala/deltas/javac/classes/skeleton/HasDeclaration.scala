package deltas.javac.classes.skeleton

import core.deltas.{Delta, HasShape}
import core.deltas.path.NodePath
import core.language.{Compilation, Language}
import core.smarts.ConstraintBuilder
import core.smarts.objects.Declaration
import core.smarts.scopes.objects.Scope
import deltas.ConstraintSkeleton

trait HasDeclaration {
  def getDeclaration(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope) : Declaration
}

trait HasDeclarationDelta extends Delta with HasShape with HasDeclaration {

  override def inject(language: Language): Unit = {
    super.inject(language)
    ConstraintSkeleton.hasDeclarations.add(language, this)
  }
}

