package core.deltas.path

import core.language.node.NodeField

trait ChildPath extends AnyPath {
  def parent: NodePath
  val field: NodeField
  def current: Any
  override def uriOption: Option[String] = parent.uriOption
}
