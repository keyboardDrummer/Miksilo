package core.particles.grammars

import java.util.NoSuchElementException

import core.biGrammar._

class GrammarCatalogue {

  var grammars: Map[Any, Labelled] = Map.empty

  def find(key: Any): Labelled = {
    try {
      grammars(key)
    } catch {
      case e: NoSuchElementException => throw GrammarNotFoundException(key, e)
    }
  }

  def create(key: AnyRef, inner: BiGrammar = BiFailure): Labelled = {
    val result = new Labelled(key, inner)
    grammars += key -> result
    result
  }
}
case class GrammarNotFoundException(key: Any, inner: Exception) extends RuntimeException(inner)
{
  override def toString = s"Could not find grammar $key."
}
