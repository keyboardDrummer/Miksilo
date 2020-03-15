package miksilo.languageServer.core.smarts.language.modules

import miksilo.languageServer.core.smarts.ConstraintBuilder
import miksilo.languageServer.core.smarts.scopes.imports.ScopeImport
import miksilo.languageServer.core.smarts.scopes.objects.Scope

class ModuleImport(name: String) extends FakeSourcePath {

  def constraints(builder: ConstraintBuilder, scope: Scope): Unit = {
    val importedDeclaration = builder.resolve(name, scope, this)
    val importedScope = builder.getDeclaredScope(importedDeclaration)

    builder.add(ScopeImport(scope, importedScope))
  }
}
