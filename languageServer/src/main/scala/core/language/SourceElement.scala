package core.language

import core.language.node.{FileRange, SourceRange}

trait SourceElement {
  def current: Any
  def position: Option[SourceRange]
  def filePosition: Option[FileRange]
}
