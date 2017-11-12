package core.particles

import core.particles.node.Node

import scala.collection.mutable

class Compilation(val language: Language) {
  var program: Node = _
  var output: String = _
  val state: mutable.Map[Any,Any] = mutable.Map.empty

  def parseString(input: String): Unit = {
    val manager = new DeltasToParserConverter()
    program = manager.parse(language.grammars.root, input).asInstanceOf[Node]
  }

  def runPhases(): Unit = {
    for(phase <- language.compilerPhases)
      phase.action(this)
  }

}

object Compilation
{
  implicit def toLanguage(compilation: Compilation): Language = compilation.language
}