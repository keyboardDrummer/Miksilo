package miksilo.languageServer.core.smarts.scopes.objects

import miksilo.languageServer.core.smarts.scopes.GraphNode
import miksilo.languageServer.server.SourcePath

case class ConcreteScope(number: Int, debugName: String = "") extends Scope with GraphNode {
  override def origin: Option[SourcePath] = None
}
