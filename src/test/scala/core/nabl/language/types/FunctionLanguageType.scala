package core.nabl.language.types

import core.nabl.constraints.ConstraintBuilder
import core.nabl.constraints.scopes.objects.Scope
import core.nabl.constraints.types.objects.Type

case class FunctionLanguageType(argument: LanguageType, result: LanguageType) extends LanguageType {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = {
    val inputType = argument.constraints(builder, scope)
    val outputType = result.constraints(builder, scope)
    builder.typesAreEqual(_type, builder.getFunctionType(inputType, outputType, this))
  }

  override def variables: Set[LanguageTypeVariable] = argument.variables ++ result.variables
}
