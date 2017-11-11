package core.particles

import java.io.InputStream

import core.bigrammar._
import core.particles.grammars.GrammarCatalogue
import core.particles.node.{GrammarKey, Node}

import scala.collection.mutable
import scala.util.Random

case class Phase(name: String, description: String, action: Compilation => Unit)





object BodyGrammar extends GrammarKey
class Language {

  object ProgramGrammar extends GrammarKey

  val data: mutable.Map[Any, Any] = mutable.Map.empty
  val grammarCatalogue = new GrammarCatalogue
  val bodyGrammar = grammarCatalogue.create(BodyGrammar, BiFailure())
  grammarCatalogue.create(ProgramGrammar, new WithTrivia(new IgnoreRight(new Sequence(bodyGrammar, grammarCatalogue.trivia)), grammarCatalogue.trivia))

  def root: Labelled = grammarCatalogue.find(ProgramGrammar)

  var compilerPhases: List[Phase] = List.empty
  var parse: InputStream => Node = _

  var random = new Random(0)
  def getUniqueLabel(prefix: String): String = prefix + getGUID

  def getGUID: Long = random.nextLong()
}
