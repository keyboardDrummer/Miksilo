package core.smarts.language.structs

import core.smarts.ConstraintBuilder
import core.smarts.language.modules.FakeSourcePath
import core.smarts.scopes.objects.Scope
import core.smarts.types.objects.{TypeFromDeclaration, Type}
import core.smarts.language.types.{LanguageType, LanguageTypeVariable}

class StructType(name: String) extends LanguageType with FakeSourcePath {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = {
    val structDeclaration = builder.resolve(name, scope, this)
    builder.typesAreEqual(_type, TypeFromDeclaration(structDeclaration))
  }

  override def variables: Set[LanguageTypeVariable] = Set.empty
}
