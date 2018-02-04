package core.nabl.language.modules

import core.nabl.ConstraintBuilder
import core.nabl.scopes.imports.ScopeImport
import core.nabl.scopes.objects.Scope

class ModuleImport(name: String) extends FakeSourceElement {

  def constraints(builder: ConstraintBuilder, scope: Scope): Unit = {
    val importedDeclaration = builder.declarationVariable()
    val importedScope = builder.resolveScopeDeclaration(importedDeclaration)
    builder.reference(name, this, scope, importedDeclaration)
    builder.add(ScopeImport(scope, importedScope))
  }
}
