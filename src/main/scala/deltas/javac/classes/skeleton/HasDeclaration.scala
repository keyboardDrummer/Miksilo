package deltas.javac.classes.skeleton

import core.deltas.path.NodePath
import core.language.Compilation
import core.smarts.ConstraintBuilder
import core.smarts.objects.Declaration
import core.smarts.scopes.objects.Scope

trait HasDeclaration {
  def getDeclaration(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope) : Declaration
}
