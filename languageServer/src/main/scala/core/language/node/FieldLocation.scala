package core.language.node

import core.language.SourceElement

case class FieldLocation(node: Node, field: NodeField) extends SourceElement { //This type has a strange name and doesn't fit nicely into the other paths.
  def current: Any = node(field)

  override def position: Option[SourceRange] = node.sources.get(field)
}
