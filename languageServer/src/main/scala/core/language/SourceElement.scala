package core.language

import core.language.node.{FileRange, SourceRange}

trait SourceElement {
  def current: Any
  /*
  A None value means the element is not part of the source.
   */
  def position: Option[SourceRange] //TODO rename to range.

  /*
  A None value means the element is not part of the source.
   */
  def filePosition: Option[FileRange] //TODO rename to range.
}
