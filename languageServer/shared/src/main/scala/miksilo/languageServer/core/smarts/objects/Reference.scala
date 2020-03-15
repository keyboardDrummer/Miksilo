package miksilo.languageServer.core.smarts.objects

import miksilo.languageServer.core.parsers.SourceElement
import miksilo.languageServer.core.smarts.scopes.GraphNode
import languageServer.SourcePath

//TODO indicate that Reference may not be a case class.
//TODO Maybe refs should have an optional origin, in case of implicit refs.
class Reference(val name: String, val origin: Option[SourcePath]) extends GraphNode
{
  override def toString = s"Reference($name, $origin)"
}
