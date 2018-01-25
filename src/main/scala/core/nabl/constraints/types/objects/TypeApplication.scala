package core.nabl.constraints.types.objects

case class PrimitiveType(name: String) extends ConcreteType {
  override def specialize(mapping: Map[TypeVariable, TypeVariable]): Type = this

  override def variables: Set[TypeVariable] = Set.empty

  override def instantiateType(variable: TypeVariable, instance: Type): Type = this

  override def fullyApplied: Boolean = true

  override def toString: String = name
}

object FuncPrimitive extends PrimitiveType("Func")

/*
 */
object FunctionType {


  def apply(argument: Type, result: Type, origin: AnyRef): Type =
    TypeApplication(PrimitiveType("Func"), Seq(argument, result), origin)

  def unapply(_type: Type): Option[(Type, Type, AnyRef)] = _type match {
    case TypeApplication(FuncPrimitive, Seq(input, output), origin) => Some(input, output, origin)
    case _ => None
  }
}

case class TypeApplication(override val function: Type, var arguments: Seq[Type], origin: AnyRef) extends ConcreteType {
  override def variables: Set[TypeVariable] = arguments.flatMap(t => t.variables).toSet

  override def specialize(mapping: Map[TypeVariable, TypeVariable]): Type = TypeApplication(function, arguments.map(a => a.specialize(mapping)), origin)

  override def instantiateType(variable: TypeVariable, instance: Type): Type = {
    TypeApplication(function.instantiateType(variable, instance), arguments.map(argument => argument.instantiateType(variable, instance)), origin)
  }

  override def fullyApplied: Boolean = arguments.forall(a => a.fullyApplied)

  override def toString: String = function + (if (arguments.nonEmpty) "<" + arguments.map(a => a.toString).reduce((a, b) => a + ", " + b) + ">" else "")

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
