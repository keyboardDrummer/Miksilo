package core.smarts

import core.smarts.objects.{Declaration, DeclarationVariable}
import core.smarts.scopes.ScopeGraph
import core.smarts.types.TypeGraph
import core.smarts.types.objects.{Type, TypeVariable}

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
