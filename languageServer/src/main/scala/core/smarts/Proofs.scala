package core.smarts

import core.language.SourceElement
import core.smarts.objects.{Declaration, NamedDeclaration, Reference}
import core.smarts.scopes.ScopeGraph
import core.smarts.types.TypeGraph
import core.smarts.types.objects.Type

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Proofs {
  val scopeGraph = new ScopeGraph
  val typeGraph = new TypeGraph
  var environment = Map.empty[Declaration, Type]
  var declarations = Map.empty[Reference, NamedDeclaration]
  var referencesPerDeclaration = mutable.Map.empty[NamedDeclaration, ArrayBuffer[Reference]]

  def gotoDefinition(location: SourceElement): Option[SourceElement] = {
    val maybeReference = scopeGraph.findReference(location)
    maybeReference.flatMap(reference => declarations(reference).origin)
  }

  def findReferences(location: SourceElement): Seq[Reference] = {
    val maybeDeclaration = scopeGraph.findDeclaration(location)
    maybeDeclaration.toSeq.flatMap(
      declaration => referencesPerDeclaration.getOrElse(declaration, ArrayBuffer.empty))
  }

  def getDeclarationsInScope(location: SourceElement): Seq[NamedDeclaration] = {
    val maybeReference = scopeGraph.findReference(location)
    val declarations = maybeReference.map(reference => scopeGraph.resolveWithoutNameCheck(reference)).getOrElse(Seq.empty)
    declarations.filter(declaration => declaration.origin.nonEmpty)
  }

  def addResolution(reference: Reference, declaration: NamedDeclaration): Unit = {
    declarations += reference -> declaration
    val references = referencesPerDeclaration.getOrElseUpdate(declaration, ArrayBuffer.empty)
    references.append(reference)
  }
}