package core.language

import core.language.node.Node
import core.smarts.{Constraint, Proofs}
import langserver.types.Diagnostic

import scala.collection.mutable

class Compilation(val language: Language) {
  var program: Node = _
  var proofs: Proofs = _
  var remainingConstraints: Seq[Constraint] = _
  var diagnostics: List[Diagnostic] = List.empty

  var output: String = _
  val state: mutable.Map[Any,Any] = mutable.Map.empty

  def runPhases(): Unit = {
    for(phase <- language.compilerPhases)
      phase.action(this)
  }
}

object Compilation
{
  implicit def toLanguage(compilation: Compilation): Language = compilation.language
}