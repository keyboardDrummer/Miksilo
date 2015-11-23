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

  def getGrammarPath(ancestor: Any, grammarKeyToFind: Any): GrammarSelection = {
    val attributeGrammar = find(ancestor)
    val rootGrammar = new RootGrammar(attributeGrammar)
    val targetGrammar = find(grammarKeyToFind)
    rootGrammar.descentsIncludingSelf.filter(path => path.get == targetGrammar).head.asInstanceOf[GrammarSelection]
  }
}
case class GrammarNotFoundException(key: Any, inner: Exception) extends RuntimeException(inner)
{
  override def toString = s"Could not find grammar $key."
}
