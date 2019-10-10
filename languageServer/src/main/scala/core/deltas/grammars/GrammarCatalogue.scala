package core.deltas.grammars

import java.util.NoSuchElementException

import core.bigrammar._
import core.bigrammar.grammars.{BiFailure, Labelled}
import core.language.node.{GrammarKey, Node}

class GrammarCatalogue {
  var grammars: Map[Any, Labelled[_]] = Map.empty

  def findAs(key: GrammarKey): Labelled[WithMap[Unit]] = find(key).asInstanceOf[Labelled[WithMap[Unit]]]
  def findNode(key: GrammarKey): Labelled[Node] = find(key).asInstanceOf[Labelled[Node]]
  def findMap(key: GrammarKey): Labelled[WithMap[_]] = find(key).asInstanceOf[Labelled[WithMap[_]]]
  def findNodeMap(key: GrammarKey): Labelled[WithMap[Node]] = find(key).asInstanceOf[Labelled[WithMap[Node]]]

  def find(key: GrammarKey): Labelled[_] = {
    try {
      grammars(key)
    } catch {
      case e: NoSuchElementException => throw GrammarNotFoundException(key, e)
    }
  }

  def create[T](key: GrammarKey, inner: BiGrammar[T] = BiFailure()): Labelled[T] = {
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
