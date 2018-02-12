package core.deltas.path

import core.language.SourceElement
import core.language.node.{Node, NodeField}

trait ChildPath extends NodePath with SourceElement {
  val current: Node
  def parent: NodePath
  def replaceWith(replacement: Any): Unit
  val field: NodeField
}
