package miksilo.languageServer.core.smarts.objects

import miksilo.languageServer.server.SourcePath
import miksilo.languageServer.core.smarts.scopes.GraphNode

class NamedDeclaration(val name: String, val origin: Option[SourcePath]) extends Declaration with GraphNode
{
  override def toString = s"NamedDeclaration($name, $origin)"
}
