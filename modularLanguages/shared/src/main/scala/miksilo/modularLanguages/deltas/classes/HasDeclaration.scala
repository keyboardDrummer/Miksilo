package miksilo.modularLanguages.deltas.javac.classes.skeleton

import miksilo.modularLanguages.core.deltas.{Delta, HasShape}
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.languageServer.core.language.{Compilation, Language}
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.objects.Declaration
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.modularLanguages.deltas.ConstraintSkeleton

trait HasDeclaration {
  def getDeclaration(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope) : Declaration
}

trait HasDeclarationDelta extends Delta with HasShape with HasDeclaration {

  override def inject(language: Language): Unit = {
    super.inject(language)
    ConstraintSkeleton.hasDeclarations.add(language, this)
  }
}

