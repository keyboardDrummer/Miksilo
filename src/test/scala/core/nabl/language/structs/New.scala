package core.nabl.language.structs

import core.nabl.constraints.ConstraintBuilder
import core.nabl.constraints.objects.Declaration
import core.nabl.constraints.scopes.objects.Scope
import core.nabl.constraints.types.InstantiateDeclarationConstraint
import core.nabl.constraints.types.objects.{StructConstraintType, Type}
import core.nabl.language.expressions.Expression
import core.nabl.language.types.LanguageType

case class New(structName: String, fieldInitializers: Seq[StructFieldInit], genericTypeArgument: Option[LanguageType] = None) extends Expression
{
  override def constraints(builder: ConstraintBuilder, _type: Type, parentScope: Scope): Unit = {
    val structDeclaration = builder.declarationVariable()
    builder.reference(structName, this, parentScope, structDeclaration)
    val structType: Type = genericTypeArgument.fold[Type](StructConstraintType(structDeclaration))((t: LanguageType) => {
      val typeArgument = t.constraints(builder, parentScope)
      val instantiatedStruct = builder.declarationVariable()
      builder.add(InstantiateDeclarationConstraint(typeArgument, instantiatedStruct, structDeclaration))
      StructConstraintType(instantiatedStruct) //TypeApplication(StructType(instantiatedStruct), Seq(typeArgument))
    })
    val instantiatedStructDeclaration: Declaration = structType.function.asInstanceOf[StructConstraintType].declaration
    val structScope = builder.declaredScopeVariable(instantiatedStructDeclaration)
    builder.typesAreEqual(_type, structType)
    fieldInitializers.foreach(value => value.constraints(builder, structScope, parentScope))
  }
}
