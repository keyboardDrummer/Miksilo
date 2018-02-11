package core.smarts.objects

import core.language.SourceElement

class NamedDeclaration(val name: String, val origin: SourceElement) extends Declaration
{
  override def toString = s"NamedDeclaration($name, $origin)"
}
