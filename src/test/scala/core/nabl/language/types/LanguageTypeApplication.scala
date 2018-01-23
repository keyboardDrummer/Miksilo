package core.nabl.language.types

import core.nabl.constraints.ConstraintBuilder
import core.nabl.constraints.scopes.objects.Scope
import core.nabl.constraints.types.objects.{Type, TypeApplication}

case class LanguageTypeApplication(function: LanguageType, argument: LanguageType) extends LanguageType {
  override def variables: Set[LanguageTypeVariable] = function.variables ++ argument.variables

  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = {
    val functionType = function.constraints(builder, scope)
    val argumentType = argument.constraints(builder, scope)
    builder.typesAreEqual(TypeApplication(functionType, Seq(argumentType), this), _type) //TODO dit snap ik niet. Type Application op een function type???
  }
}
