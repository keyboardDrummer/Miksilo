package core.nabl.language.structs

import core.nabl.ConstraintBuilder
import core.nabl.objects.Declaration
import core.nabl.scopes.objects.Scope
import core.nabl.types.InstantiateDeclarationConstraint
import core.nabl.types.objects.{TypeFromDeclaration, Type}
import core.nabl.language.expressions.Expression
import core.nabl.language.types.LanguageType

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
    val structScope = builder.declaredScopeVariable(instantiatedStructDeclaration)
    builder.typesAreEqual(_type, structType)
    fieldInitializers.foreach(value => value.constraints(builder, structScope, parentScope))
  }
}
