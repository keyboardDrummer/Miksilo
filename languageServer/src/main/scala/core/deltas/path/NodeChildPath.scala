package core.deltas.path

import core.language.node.{Node, NodeField}

trait ChildPath extends AnyPath {
  def replaceWith(replacement: Any): Unit
  def parent: NodePath
  val field: NodeField
  def current: Any
  override def uriOption: Option[String] = parent.uriOption
}

trait NodeChildPath extends NodePath with ChildPath {
  def current: Node
  override def uriOption: Option[String] = super[NodePath].uriOption.orElse(super[ChildPath].uriOption)
}
