package core.deltas.path

import core.deltas.node.Node

trait ChildPath extends Path with SourceElementWithValue {
  val current: Node
  def parent: Path
  def replaceWith(replacement: Any): Unit
}
