package core.nabl.objects

class Reference(val name: String, val id: AnyRef)
{
  override def toString = s"Reference($name, $id)"
}
