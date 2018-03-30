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
    val maybeReference = scopeGraph.findReference(location)
    maybeReference.flatMap(reference => resolutions(reference).origin)
  }

  def getDeclarationsInScope(location: SourceElement): Seq[NamedDeclaration] = {
    val maybeReference = scopeGraph.findReference(location)
    val declarations = maybeReference.map(reference => scopeGraph.resolveWithoutNameCheck(reference)).getOrElse(Seq.empty)
    declarations.filter(declaration => declaration.origin.nonEmpty)
  }
}