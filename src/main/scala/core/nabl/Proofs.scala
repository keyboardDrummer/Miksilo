package core.nabl

import core.nabl.objects.{Declaration, DeclarationVariable}
import core.nabl.scopes.ScopeGraph
import core.nabl.types.TypeGraph
import core.nabl.types.objects.{Type, TypeVariable}

trait Proofs {
  def scopeGraph: ScopeGraph
  def typeGraph: TypeGraph
  def environment: Map[Declaration, Type]
  def mappedTypeVariables: Map[TypeVariable, Type]
  def mappedDeclarationVariables: Map[DeclarationVariable, Declaration]
}
