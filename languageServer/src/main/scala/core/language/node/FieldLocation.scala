package core.language.node

import core.deltas.path.NodePath
import core.language.SourceElement

case class FieldLocation(path: NodePath, field: NodeField) extends SourceElement { //This type has a strange name and doesn't fit nicely into the other paths.
  def current: Any = path.current(field)

  override def position: Option[SourceRange] = path.current.sources.get(field)

  override def filePosition: Option[FileRange] = position.flatMap(p => path.uriOption.map(u => FileRange(u, p)))
}
