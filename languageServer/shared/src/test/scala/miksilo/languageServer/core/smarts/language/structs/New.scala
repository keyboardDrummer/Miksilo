package miksilo.languageServer.core.smarts.language.structs

import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.objects.Declaration
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.languageServer.core.smarts.types.InstantiateDeclarationConstraint
import miksilo.languageServer.core.smarts.types.objects.{TypeFromDeclaration, Type}
import miksilo.languageServer.core.smarts.language.expressions.Expression
import miksilo.languageServer.core.smarts.language.types.LanguageType

case class New(structName: String, fieldInitializers: Seq[StructFieldInit], genericTypeArgument: Option[LanguageType] = None) extends Expression
{
  override def constraints(builder: ConstraintBuilder, _type: Type, parentScope: Scope): Unit = {
    val structDeclaration = builder.resolve(structName, parentScope, this)
    val structType: Type = genericTypeArgument.fold[Type](TypeFromDeclaration(structDeclaration))((t: LanguageType) => {
      val typeArgument = t.constraints(builder, parentScope)
      val instantiatedStruct = builder.declarationVariable()
      builder.add(InstantiateDeclarationConstraint(typeArgument, instantiatedStruct, structDeclaration))
      TypeFromDeclaration(instantiatedStruct) //TypeApplication(StructType(instantiatedStruct), Seq(typeArgument))
    })
    val instantiatedStructDeclaration: Declaration = structType.function.asInstanceOf[TypeFromDeclaration].declaration
    val structScope = builder.getDeclaredScope(instantiatedStructDeclaration)
    builder.typesAreEqual(_type, structType)
    fieldInitializers.foreach(value => value.constraints(builder, structScope, parentScope))
  }
}
