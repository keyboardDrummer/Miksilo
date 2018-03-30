package core.smarts.objects

import core.language.SourceElement

//TODO indicate that Reference may not be a case class.
class Reference(val name: String, val origin: Option[SourceElement]) //TODO Maybe refs should have an optional origin, in case of implicit refs.
{
  override def toString = s"Reference($name, $origin)"
}
