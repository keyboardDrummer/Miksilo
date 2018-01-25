package core.nabl.language.types

import core.nabl.ConstraintBuilder
import core.nabl.scopes.objects.Scope
import core.nabl.types.objects.Type

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
