package miksilo.modularLanguages.core.deltas.path

import miksilo.modularLanguages.core.node.NodeField

trait ChildPath extends AnyPath {
  def replaceWith(replacement: Any): Unit
  def parent: NodePath
  val field: NodeField
  def current: Any
  override def uriOption: Option[String] = parent.uriOption
}
