package core.particles

import java.io.InputStream

import core.bigrammar.BiFailure
import core.particles.grammars.{GrammarCatalogue, ProgramGrammar}
import core.particles.node.Node

import scala.collection.mutable
import scala.util.Random

object ParseUsingTextualGrammar extends Delta {
  override def inject(state: Language): Unit = {
    val grammarCatalogue = state.grammarCatalogue
    state.parse = input => {
      val inputString = scala.io.Source.fromInputStream(input).mkString
      val manager = new DeltasToParserConverter()
      manager.parse(grammarCatalogue.find(ProgramGrammar), inputString).asInstanceOf[Node]
    }
  }

  override def description: String = "Parses the input file using a textual grammar."
}

case class Phase(name: String, description: String, action: Compilation => Unit)

object Compilation
{
  implicit def toLanguage(compilation: Compilation): Language = compilation.language
}

class Compilation(val language: Language) {
  var program: Node = _
  var output: String = _
  val state: mutable.Map[Any,Any] = mutable.Map.empty

  def parseString(input: String): Unit = {
    val manager = new DeltasToParserConverter()
    program = manager.parse(language.grammarCatalogue.find(ProgramGrammar), input).asInstanceOf[Node]
  }

  def runPhases(): Unit = {
    for(phase <- language.compilerPhases)
      phase.action(this)
  }

}

class Language {
  val data: mutable.Map[Any, Any] = mutable.Map.empty
  val grammarCatalogue = new GrammarCatalogue
  grammarCatalogue.create(ProgramGrammar, BiFailure())
  var compilerPhases: List[Phase] = List.empty
  var parse: InputStream => Node = _

  var random = new Random(0)
  def getUniqueLabel(prefix: String): String = prefix + getGUID

  def getGUID: Long = random.nextLong()
}
