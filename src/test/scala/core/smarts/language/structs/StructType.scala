package core.smarts.language.structs

import core.smarts.ConstraintBuilder
import core.smarts.language.modules.FakeSourceElement
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.{TypeFromDeclaration, Type}
import core.smarts.language.types.{LanguageType, LanguageTypeVariable}

class StructType(name: String) extends LanguageType with FakeSourceElement {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = {
    val structDeclaration = builder.declarationVariable()
    builder.reference(name, this, scope, structDeclaration)
    builder.typesAreEqual(_type, TypeFromDeclaration(structDeclaration))
  }

  override def variables: Set[LanguageTypeVariable] = Set.empty
}
