package core.smarts.objects

import core.language.SourceElement

case class Reference(name: String, origin: Option[SourceElement]) //TODO Maybe refs should have an optional origin, in case of implicit refs.
{
  override def toString = s"Reference($name, $origin)"
}
