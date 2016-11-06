package core.particles.grammars

import java.util.NoSuchElementException

import core.bigrammar._
import core.particles.node.Key

case class KeyGrammar(key: Key)
{
  override def toString = key.toString
}

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

  def findPathsToKey(rootKey: Any, grammarKeyToFind: Any): Seq[GrammarSelection] = { //TODO GrammarKey requiren
    val attributeGrammar = find(rootKey)
    val rootGrammar = new RootGrammar(attributeGrammar)
    val targetGrammar = find(grammarKeyToFind)
    rootGrammar.descentsIncludingSelf.filter(path => path.get == targetGrammar).collect { case x: GrammarSelection => x }
  }
}
case class GrammarNotFoundException(key: Any, inner: Exception) extends RuntimeException(inner)
{
  override def toString = s"Could not find grammar $key."
}
