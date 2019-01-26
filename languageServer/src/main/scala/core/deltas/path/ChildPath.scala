package core.deltas.path

import core.language.node.NodeField

trait ChildPath extends AnyPath {
  def replaceWith(replacement: Any): Unit
  def parent: NodePath
  val field: NodeField
  def keyFromParent: Any
  def current: Any
  override def uriOption: Option[String] = parent.uriOption
}
