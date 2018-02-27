package core.smarts

import core.language.SourceElement
import core.smarts.objects.{Declaration, NamedDeclaration, Reference}
import core.smarts.scopes.ScopeGraph
import core.smarts.types.TypeGraph
import core.smarts.types.objects.Type

class Proofs {
  val scopeGraph = new ScopeGraph
  val typeGraph = new TypeGraph
  var environment = Map.empty[Declaration, Type]
  var resolutions = Map.empty[Reference, NamedDeclaration]

  def resolveLocation(location: SourceElement): Option[SourceElement] = {
    val reference = scopeGraph.findReference(location).get
    resolutions(reference).origin
  }
}