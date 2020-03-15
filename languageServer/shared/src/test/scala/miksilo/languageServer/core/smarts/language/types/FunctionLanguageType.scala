package miksilo.languageServer.core.smarts.language.types
import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.languageServer.core.smarts.types.objects.{FunctionType, Type}

case class FunctionLanguageType(argument: LanguageType, result: LanguageType) extends LanguageType {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = {
    val inputType = argument.constraints(builder, scope)
    val outputType = result.constraints(builder, scope)
    builder.typesAreEqual(_type, FunctionType(inputType, outputType, Some(this)))
  }

  override def variables: Set[LanguageTypeVariable] = argument.variables ++ result.variables
}
