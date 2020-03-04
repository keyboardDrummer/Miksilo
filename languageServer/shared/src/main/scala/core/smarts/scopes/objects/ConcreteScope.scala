package core.smarts.scopes.objects

import core.language.SourceElement
import core.smarts.scopes.GraphNode

case class ConcreteScope(number: Int, debugName: String = "") extends Scope with GraphNode {
  override def origin: Option[SourceElement] = None
}
