package core.smarts.language.structs

import core.smarts.ConstraintBuilder
import core.smarts.objects.Declaration
import core.smarts.scopes.objects.Scope
import core.smarts.types.InstantiateDeclarationConstraint
import core.smarts.types.objects.{TypeFromDeclaration, Type}
import core.smarts.language.expressions.Expression
import core.smarts.language.types.LanguageType

case class New(structName: String, fieldInitializers: Seq[StructFieldInit], genericTypeArgument: Option[LanguageType] = None) extends Expression
{
  override def constraints(builder: ConstraintBuilder, _type: Type, parentScope: Scope): Unit = {
    val structDeclaration = builder.declarationVariable()
    builder.reference(structName, this, parentScope, structDeclaration)
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
