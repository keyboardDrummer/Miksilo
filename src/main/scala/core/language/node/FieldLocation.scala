package core.language.node

import core.language.SourceElement

case class FieldLocation(node: Node, field: NodeField) extends SourceElement {
  def current: Any = node(field)

  override def position: Option[SourceRange] = node.sources.get(field)
}
