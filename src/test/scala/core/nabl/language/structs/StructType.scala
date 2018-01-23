package core.nabl.language.structs

import core.nabl.constraints.ConstraintBuilder
import core.nabl.constraints.scopes.objects.Scope
import core.nabl.constraints.types.objects.{StructConstraintType, Type}
import core.nabl.language.types.{LanguageType, LanguageTypeVariable}

class StructType(name: String) extends LanguageType {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = {
    val structDeclaration = builder.declarationVariable()
    builder.reference(name, this, scope, structDeclaration)
    builder.typesAreEqual(_type, StructConstraintType(structDeclaration))
  }

  override def variables: Set[LanguageTypeVariable] = Set.empty
}
