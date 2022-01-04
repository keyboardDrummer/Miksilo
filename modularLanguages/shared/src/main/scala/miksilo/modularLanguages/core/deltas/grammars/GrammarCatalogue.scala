package miksilo.modularLanguages.core.deltas.grammars

import java.util.NoSuchElementException

import miksilo.modularLanguages.core.bigrammar.{BiGrammar, GrammarReference, RootGrammar}
import miksilo.modularLanguages.core.bigrammar.grammars.{BiFailure, Labelled}
import miksilo.modularLanguages.core.node.GrammarKey

class GrammarCatalogue {
  var grammars: Map[Any, Labelled] = Map.empty
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

  def findPath(from: GrammarKey, to: GrammarKey): GrammarReference = {
    val rootGrammar = new RootGrammar(find(from))
    rootGrammar.findLabelled(to)
  }
}

case class GrammarNotFoundException(key: Any, inner: Exception) extends RuntimeException(inner)
{
  override def toString = s"Could not find grammar $key."
}
