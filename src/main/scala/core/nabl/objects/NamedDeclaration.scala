package core.nabl.objects

class NamedDeclaration(val name: String, val id: AnyRef) extends Declaration
{
  override def toString = s"NamedDeclaration($name, $id)"
}
