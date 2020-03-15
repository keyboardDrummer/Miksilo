package miksilo.modularLanguages.core.deltas.path

import miksilo.modularLanguages.core.node.Node
import miksilo.editorParser.parsers.editorParsers.{OffsetPointerRange, OffsetRange, SourceRange}

trait NodeChildPath extends NodePath with ChildPath {
  def current: Node
  override def uriOption: Option[String] = super[NodePath].uriOption.orElse(super[ChildPath].uriOption)

  override def rangeOption: Option[OffsetPointerRange] = current.asNode.range
}
