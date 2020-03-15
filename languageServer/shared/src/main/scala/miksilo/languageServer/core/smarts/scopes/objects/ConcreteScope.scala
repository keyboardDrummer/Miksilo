package miksilo.languageServer.core.smarts.scopes.objects

import miksilo.languageServer.core.smarts.scopes.GraphNode
import languageServer.SourcePath

case class ConcreteScope(number: Int, debugName: String = "") extends Scope with GraphNode {
  override def origin: Option[SourcePath] = None
}
