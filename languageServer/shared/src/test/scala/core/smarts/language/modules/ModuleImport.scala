package core.smarts.language.modules

import core.smarts.ConstraintBuilder
import core.smarts.scopes.imports.ScopeImport
import core.smarts.scopes.objects.Scope

class ModuleImport(name: String) extends FakeSourceElement {

  def constraints(builder: ConstraintBuilder, scope: Scope): Unit = {
    val importedDeclaration = builder.resolve(name, scope, this)
    val importedScope = builder.getDeclaredScope(importedDeclaration)

    builder.add(ScopeImport(scope, importedScope))
  }
}
