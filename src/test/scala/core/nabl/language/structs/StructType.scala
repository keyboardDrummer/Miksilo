package core.nabl.language.structs

import core.nabl.ConstraintBuilder
import core.nabl.language.modules.FakeSourceElement
import core.nabl.scopes.objects.Scope
import core.nabl.types.objects.{TypeFromDeclaration, Type}
import core.nabl.language.types.{LanguageType, LanguageTypeVariable}

class StructType(name: String) extends LanguageType with FakeSourceElement {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = {
    val structDeclaration = builder.declarationVariable()
    builder.reference(name, this, scope, structDeclaration)
    builder.typesAreEqual(_type, TypeFromDeclaration(structDeclaration))
  }

  override def variables: Set[LanguageTypeVariable] = Set.empty
}
