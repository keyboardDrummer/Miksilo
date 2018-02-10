package deltas.javac.classes.skeleton

import core.deltas.Compilation
import core.deltas.node.NodeShape
import core.deltas.path.Path
import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope

trait ShapeWithConstraints extends NodeShape {
  def collectConstraints(compilation: Compilation, builder: ConstraintBuilder, path: Path, parentScope: Scope) : Unit
}
