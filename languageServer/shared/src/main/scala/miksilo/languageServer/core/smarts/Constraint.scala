package miksilo.languageServer.core.smarts

import miksilo.languageServer.core.language.Compilation
import miksilo.languageServer.core.smarts.objects.{Declaration, DeclarationVariable}
import miksilo.languageServer.core.smarts.scopes.objects.{Scope, ScopeVariable}
import miksilo.languageServer.core.smarts.types.objects.{Type, TypeVariable}
import miksilo.lspprotocol.lsp.Diagnostic

trait Constraint {
  def apply(solver: ConstraintSolver): Boolean

  def instantiateDeclaration(variable: DeclarationVariable, instance: Declaration): Unit = {}
  def instantiateType(variable: TypeVariable, instance: Type): Unit = {}
  def instantiateScope(variable: ScopeVariable, instance: Scope): Unit = {}
  def boundTypes: Set[Type] = Set.empty

  def getDiagnostic(compilation: Compilation): Option[FileDiagnostic] = None
}

case class FileDiagnostic(uri: String, diagnostic: Diagnostic)