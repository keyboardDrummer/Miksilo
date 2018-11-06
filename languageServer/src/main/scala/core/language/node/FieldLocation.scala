package core.language.node

import core.deltas.path.NodePath
import core.language.SourceElement

case class FieldLocation(path: NodePath, field: NodeField) extends SourceElement { // TODO, rename to FieldPath.
  def current: Any = path.current(field)

  override def position: Option[SourceRange] = path.current.sources.get(field)

  override def filePosition: Option[FileRange] = position.flatMap(p => path.uriOption.map(u => FileRange(u, p)))
}
