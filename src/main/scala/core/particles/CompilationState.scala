package core.particles

import java.io.InputStream

import core.bigrammar.BiFailure
import core.grammar.ParseException
import core.particles.grammars.{GrammarCatalogue, ProgramGrammar}
import core.particles.node.{Node, Node$}

import scala.collection.mutable
import scala.util.Random

object ParseUsingTextualGrammar extends Delta {
  override def inject(state: CompilationState): Unit = {
    val grammarCatalogue = state.grammarCatalogue
    state.parse = input => {
      val inputString = scala.io.Source.fromInputStream(input).mkString
      val manager = new DeltasToParserConverter()
      manager.parse(grammarCatalogue.find(ProgramGrammar), inputString).asInstanceOf[Node]
    }
  }

  override def description: String = "Parses the input file using a textual grammar."
}

case class Phase(name: String, description: String, action: () => Unit)

class CompilationState {
  var output: String = null
  val data: mutable.Map[Any, Any] = mutable.Map.empty
  val grammarCatalogue = new GrammarCatalogue
  grammarCatalogue.create(ProgramGrammar, BiFailure())
  var program: Node = null
  var compilerPhases: List[Phase] = List.empty
  var parse: InputStream => Node = null

  var random = new Random(0)
  def getUniqueLabel(prefix: String) = prefix + getGUID

  def getGUID: Long = random.nextLong()

  def parseString(input: String): Unit = {
    val manager = new DeltasToParserConverter()
    program = manager.parse(grammarCatalogue.find(ProgramGrammar), input).asInstanceOf[Node]
  }

  def runPhases() = {
    for(phase <- compilerPhases)
      phase.action()
  }
}
