package core.language

import core.language.node.SourceRange

trait SourceElement {
  def current: Any
  def position: Option[SourceRange]
}
