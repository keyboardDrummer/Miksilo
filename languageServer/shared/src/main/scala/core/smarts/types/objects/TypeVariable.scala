package core.smarts.types.objects

import languageServer.SourcePath

case class TypeVariable(name: String, origin: Option[SourcePath] = None) extends Type {
  override def variables: Set[TypeVariable] = Set(this)

  override def specialize(mapping: Map[TypeVariable, TypeVariable]): Type = mapping(this)

  override def instantiateType(variable: TypeVariable, instance: Type): Type = if (this == variable) instance else this

  override def fullyApplied: Boolean = false
}
