package core.smarts.objects

import core.language.SourceElement
import core.smarts.scopes.GraphNode

class NamedDeclaration(val name: String, val origin: Option[SourceElement]) extends Declaration with GraphNode
{
  override def toString = s"NamedDeclaration '$name'"
}
