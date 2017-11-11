package core.particles

import java.io.InputStream

import core.bigrammar.{BiFailure, IgnoreRight, Sequence, WithTrivia}
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{GrammarKey, Node}

import scala.collection.mutable
import scala.util.Random

object ParseUsingTextualGrammar extends Delta {
  override def inject(state: Language): Unit = {
    val grammarCatalogue = state.grammars
    state.parse = input => {
      val inputString = scala.io.Source.fromInputStream(input).mkString
      val manager = new DeltasToParserConverter()
      manager.parse(grammarCatalogue.root, inputString).asInstanceOf[Node]
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
    program = manager.parse(language.grammars.root, input).asInstanceOf[Node]
  }

  def runPhases(): Unit = {
    for(phase <- language.compilerPhases)
      phase.action(this)
  }

}

object BodyGrammar extends GrammarKey
class Language {
  val data: mutable.Map[Any, Any] = mutable.Map.empty
  val grammars = new GrammarCatalogue

  val bodyGrammar = grammars.create(BodyGrammar, BiFailure())
  grammars.root = new WithTrivia(new IgnoreRight(new Sequence(bodyGrammar, grammars.trivia)), grammars.trivia)

  var compilerPhases: List[Phase] = List.empty
  var parse: InputStream => Node = _

  var random = new Random(0)
  def getUniqueLabel(prefix: String): String = prefix + getGUID

  def getGUID: Long = random.nextLong()
}
