package core.particles.grammars

import java.util.NoSuchElementException

import core.bigrammar._
import core.particles.node.Key

case class KeyGrammar(key: Key) extends Key
{
  override lazy val toString = key.toString
}

class GrammarCatalogue {

  var grammars: Map[Any, Labelled] = Map.empty

  def root: BiGrammar = grammars(ProgramGrammar)
  def find(key: Any): Labelled = {
    try {
      grammars(key)
    } catch {
      case e: NoSuchElementException => throw GrammarNotFoundException(key, e)
    }
  }

  def create(key: AnyRef, inner: BiGrammar = BiFailure()): Labelled = {
    val result = new Labelled(key, inner)
    grammars += key -> result
    result
  }

  def findPath(to: Key, from: Key): GrammarReference = {
    val rootGrammar = new RootGrammar(find(from))
    rootGrammar.findLabelled(to)
  }
}

case class GrammarNotFoundException(key: Any, inner: Exception) extends RuntimeException(inner)
{
  override def toString = s"Could not find grammar $key."
}
