package core.deltas.path

import core.deltas.node.Node

trait ChildPath extends Path with AnyChildPath {
  val current: Node
  def parent: Path
  def replaceWith(replacement: Any): Unit
}
