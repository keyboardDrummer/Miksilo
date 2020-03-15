package miksilo.languageServer.core.smarts.objects

import miksilo.languageServer.core.parsers.SourceElement
import miksilo.languageServer.core.smarts.scopes.GraphNode
import languageServer.SourcePath

class NamedDeclaration(val name: String, val origin: Option[SourcePath]) extends Declaration with GraphNode
{
  override def toString = s"NamedDeclaration($name, $origin)"
}
