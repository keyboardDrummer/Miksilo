package core.language

import core.language.node.{FileRange, SourceRange}

trait SourceElement {
  def current: Any
  /*
  A None value means the element is not part of the source.
   */
  def range: Option[SourceRange]

  /*
  A None value means the element is not part of the source.
   */
  def fileRange: Option[FileRange]
}
