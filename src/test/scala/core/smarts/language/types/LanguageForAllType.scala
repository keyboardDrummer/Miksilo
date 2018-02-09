package core.smarts.language.types

import core.smarts.ConstraintBuilder
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.Type

case class LanguageForAllType(variable: String, body: LanguageType) extends LanguageType {

  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = {
//    if (body.variables != Set(LanguageTypeVariable(variable)))
//    {
//      throw TypeCheckException("language forall variables don't add up") //TODO gekke check tijdens constraint collectie
//    }
    body.constraints(builder, _type, scope)
  }

  override def variables: Set[LanguageTypeVariable] = body.variables.diff(Set(LanguageTypeVariable(variable)))
}
