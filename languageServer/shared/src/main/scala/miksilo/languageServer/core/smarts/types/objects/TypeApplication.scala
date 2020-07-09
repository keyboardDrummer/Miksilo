package miksilo.languageServer.core.smarts.types.objects

case class TypeApplication(override val function: Type, var arguments: Seq[Type], origin: AnyRef) extends ConcreteType {

  override def variables: Set[TypeVariable] = arguments.flatMap(t => t.variables).toSet

  override def specialize(mapping: Map[TypeVariable, TypeVariable]): Type = TypeApplication(function, arguments.map(a => a.specialize(mapping)), origin)

  override def instantiateType(variable: TypeVariable, instance: Type): Type = {
    TypeApplication(function.instantiateType(variable, instance), arguments.map(argument => argument.instantiateType(variable, instance)), origin)
  }

  override def fullyApplied: Boolean = arguments.forall(a => a.fullyApplied)

  override def toString: String = {
    val typeArgument = if (arguments.nonEmpty) "<" + arguments.map(a => a.toString).mkString(", ") + ">" else ""
    s"$function$typeArgument"
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[TypeApplication]

  override def equals(other: Any): Boolean = other match {
    case that: TypeApplication =>
      (that canEqual this) &&
        function == that.function &&
        arguments == that.arguments
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(function, arguments)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}
