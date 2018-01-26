package core.nabl.objects

import core.language.SourceElement

class Reference(val name: String, val origin: SourceElement) //TODO Maybe refs should have an optional origin, in case of implicit refs.
{
  override def toString = s"Reference($name, $origin)"
}
