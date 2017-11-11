package core.particles.grammars

import java.util.NoSuchElementException

import core.bigrammar._
import core.particles.GrammarWithTrivia
import core.particles.node.{GrammarKey, Key}

case class KeyGrammar(key: Key) extends GrammarKey
{
  override lazy val toString = key.toString
}


object TriviaGrammar extends GrammarKey
class GrammarCatalogue {

  var grammars: Map[Any, Labelled] = Map.empty

  implicit def stringToAstGrammar(value: String) = new GrammarWithTrivia(Keyword(value))(this)
  implicit def grammarToAstGrammar(value: BiGrammar) = new GrammarWithTrivia(value)(this)

  val trivia: Labelled = new Labelled(TriviaGrammar, ParseWhiteSpace) //TODO can we move this and the trivia part outside of grammarCatalogue to make it more generic?

  def find(key: GrammarKey): Labelled = {
    try {
      grammars(key)
    } catch {
      case e: NoSuchElementException => throw GrammarNotFoundException(key, e)
    }
  }

  def create(key: GrammarKey, inner: BiGrammar = BiFailure()): Labelled = {
    val result = new Labelled(key, inner)
    grammars += key -> result
    result
  }

  def findPath(to: GrammarKey, from: GrammarKey): GrammarReference = {
    val rootGrammar = new RootGrammar(find(from))
    rootGrammar.findLabelled(to)
  }
}

case class GrammarNotFoundException(key: Any, inner: Exception) extends RuntimeException(inner)
{
  override def toString = s"Could not find grammar $key."
}
