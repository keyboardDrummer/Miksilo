package miksilo.modularLanguages.deltas

import miksilo.modularLanguages.core.deltas.ShapeProperty
import miksilo.modularLanguages.core.deltas.path.NodePath
import miksilo.languageServer.core.language.Compilation
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.modularLanguages.deltas.classes.HasConstraints
import miksilo.modularLanguages.deltas.javac.classes.skeleton.HasDeclaration

object ConstraintSkeleton {

  val hasDeclarations: ShapeProperty[HasDeclaration] = new ShapeProperty[HasDeclaration]
  val hasConstraints: ShapeProperty[HasConstraints] = new ShapeProperty[HasConstraints]

  def constraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope): Unit = {
    hasConstraints(compilation, path.shape).collectConstraints(compilation, builder, path, parentScope)
  }
}
