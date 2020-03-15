package miksilo.languageServer.core.smarts.types.objects

object FunctionType {

  def curry(arguments: Seq[Type], initialResult: Type = null): Type = {
    var result: Type = initialResult
    for(parameter <- arguments.reverse) {
      result = FunctionType(parameter, result, None)
    }
    result
  }

  def apply(argument: Type, result: Type, origin: Option[AnyRef]): Type =
    TypeApplication(FuncPrimitive, Seq(argument, result), origin)

  def unapply(_type: Type): Option[(Type, Type, AnyRef)] = _type match {
    case TypeApplication(FuncPrimitive, Seq(input, output), origin) => Some(input, output, origin)
    case _ => None
  }
}