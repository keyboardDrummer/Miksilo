package core.deltas.path

import core.language.SourceElement

trait AnyChildPath extends SourceElement {
  def current: Any
}
