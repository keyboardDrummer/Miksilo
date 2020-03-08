package core.deltas.path

import core.language.node.Node
import core.parsers.editorParsers.{OffsetNodeRange, OffsetRange, SourceRange}

trait NodeChildPath extends NodePath with ChildPath {
  def current: Node
  override def uriOption: Option[String] = super[NodePath].uriOption.orElse(super[ChildPath].uriOption)

  override def range: Option[OffsetNodeRange] = current.asNode.range
}
