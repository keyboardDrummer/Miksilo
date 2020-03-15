package miksilo.languageServer.core.smarts.types.objects

case class PrimitiveType(name: Any) extends ConcreteType {
  override def specialize(mapping: Map[TypeVariable, TypeVariable]): Type = this

  override def variables: Set[TypeVariable] = Set.empty

  override def instantiateType(variable: TypeVariable, instance: Type): Type = this

  override def fullyApplied: Boolean = true

  override def toString: String = name.toString
}
