package core.smarts.objects

import core.parsers.SourceElement
import core.smarts.scopes.GraphNode
import languageServer.SourcePath

class NamedDeclaration(val name: String, val origin: Option[SourcePath]) extends Declaration with GraphNode
{
  override def toString = s"NamedDeclaration($name, $origin)"
}
