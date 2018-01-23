package core.nabl.language.modules

import core.nabl.constraints.ConstraintBuilder
import core.nabl.constraints.scopes.imports.ScopeImport
import core.nabl.constraints.scopes.objects.Scope

class ModuleImport(name: String) {

  def constraints(builder: ConstraintBuilder, scope: Scope): Unit = {
    val importedDeclaration = builder.declarationVariable()
    val importedScope = builder.declaredScopeVariable(importedDeclaration)
    builder.reference(name, this, scope, importedDeclaration)
    builder.add(ScopeImport(scope, importedScope))
  }
}
