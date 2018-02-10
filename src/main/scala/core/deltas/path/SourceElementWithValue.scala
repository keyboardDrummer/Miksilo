package core.deltas.path

import core.language.SourceElement

trait SourceElementWithValue extends SourceElement {
  def current: Any
}
