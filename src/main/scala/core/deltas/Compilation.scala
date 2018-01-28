package core.deltas

import core.deltas.node.Node
import core.language.Language
import core.nabl.BindingsAndTypes

import scala.collection.mutable

class Compilation(val language: Language) {
  var program: Node = _
  var staticState: BindingsAndTypes = _

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