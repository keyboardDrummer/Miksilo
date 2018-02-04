package core.nabl

import core.nabl.objects.{Declaration, DeclarationVariable}
import core.nabl.scopes.ScopeGraph
import core.nabl.types.TypeGraph
import core.nabl.types.objects.{Type, TypeVariable}

class Proofs extends ProofsLike {
  val scopeGraph = new ScopeGraph
  val typeGraph = new TypeGraph
  var environment = Map.empty[Declaration, Type]
}

trait ProofsLike {
  def scopeGraph: ScopeGraph
  def typeGraph: TypeGraph
  def environment: Map[Declaration, Type]
}
