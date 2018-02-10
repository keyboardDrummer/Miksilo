package core.deltas.path

import core.deltas.node.Node

trait ChildPath extends NodePath with SourceElementWithValue {
  val current: Node
  def parent: NodePath
  def replaceWith(replacement: Any): Unit
}
