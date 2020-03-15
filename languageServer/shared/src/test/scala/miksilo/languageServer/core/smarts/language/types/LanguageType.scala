package miksilo.languageServer.core.smarts.language.types

import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.languageServer.core.smarts.types.objects.Type

trait LanguageType
{
  def variables: Set[LanguageTypeVariable]

  def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit

  def constraints(builder: ConstraintBuilder, scope: Scope) : Type = {
    val result = builder.typeVariable()
    constraints(builder, result, scope)
    result
  }
}

object LanguageType
{
  implicit def variable(name: String) : LanguageTypeVariable = LanguageTypeVariable(name)
}

case class LanguageTypeVariable(name: String) extends LanguageType {

  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = {
    val typeVariable = builder.typeVariables.getOrElseUpdate(name, builder.typeVariable())
    builder.typesAreEqual(typeVariable, _type)
  }

  override def variables: Set[LanguageTypeVariable] = Set(this)
}


