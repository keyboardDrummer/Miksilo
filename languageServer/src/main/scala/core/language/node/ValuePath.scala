package core.language.node

import core.deltas.path.NodePath
import core.language.SourceElement

/**
  * A path to any value
  * @param path Path to the node that contains the value
  * @param field The field that contains the value
  */
case class ValuePath(path: NodePath, field: NodeField) extends SourceElement {
  def current: Any = path.current(field)

  override def range: Option[SourceRange] = path.current.sources.get(field)

  override def fileRange: Option[FileRange] = range.flatMap(p => path.uriOption.map(u => FileRange(u, p)))
}
