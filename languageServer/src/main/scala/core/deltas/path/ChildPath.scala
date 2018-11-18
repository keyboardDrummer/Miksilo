package core.deltas.path

import core.language.node.{Node, NodeField}

trait AnyChildPath extends AnyPath {
  def replaceWith(replacement: Any): Unit
  def parent: NodePath
  val field: NodeField
  def current: Any
  override def uriOption: Option[String] = parent.uriOption
}

trait ChildPath extends NodePath with AnyChildPath {
  def current: Node
  override def uriOption: Option[String] = super.uriOption.orElse(parent.uriOption)
}
