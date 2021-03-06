package miksilo.languageServer.core.smarts.language.structs

import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.language.modules.FakeSourcePath
import miksilo.languageServer.core.smarts.scopes.objects.Scope
import miksilo.languageServer.core.smarts.types.objects.{TypeFromDeclaration, Type}
import miksilo.languageServer.core.smarts.language.types.{LanguageType, LanguageTypeVariable}

class StructType(name: String) extends LanguageType with FakeSourcePath {
  override def constraints(builder: ConstraintBuilder, _type: Type, scope: Scope): Unit = {
    val structDeclaration = builder.resolve(name, scope, this)
    builder.typesAreEqual(_type, TypeFromDeclaration(structDeclaration))
  }

  override def variables: Set[LanguageTypeVariable] = Set.empty
}
