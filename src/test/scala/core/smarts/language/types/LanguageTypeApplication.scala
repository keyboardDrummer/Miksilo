package core.smarts.language.types

import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.{Type, TypeApplication}

case class LanguageTypeApplication(function: LanguageType, argument: LanguageType) extends LanguageType {
  override def variables: Set[LanguageTypeVariable] = function.variables ++ argument.variables

  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = {
    val functionType = function.constraints(builder, scope)
    val argumentType = argument.constraints(builder, scope)
    builder.typesAreEqual(TypeApplication(functionType, Seq(argumentType), this), _type) //TODO dit snap ik niet. Type Application op een function type???
  }
}
