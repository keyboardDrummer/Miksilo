package deltas.javac.classes.skeleton

import core.language.node.NodeShape
import core.deltas.path.NodePath
import core.language.Compilation
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope

trait ShapeWithConstraints extends NodeShape {
  def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: NodePath, parentScope: Scope) : Unit
}
