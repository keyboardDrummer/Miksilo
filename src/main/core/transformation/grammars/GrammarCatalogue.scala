package core.transformation.grammars

import java.util.NoSuchElementException

import core.grammar._

class GrammarCatalogue {


  var grammars: Map[Any, Labelled] = Map.empty

  def find(key: Any): Labelled = {
    try {
      grammars(key)
    } catch {
      case e: NoSuchElementException => throw GrammarNotFoundException(key, e)
    }
  }

  def create(key: AnyRef, inner: Grammar = FailureG): Labelled = {
    val result = new Labelled(key, inner)
    grammars += key -> result
    result
  }

  def getGrammars: Set[Grammar] = {

    var closed = Set.empty[Grammar]
    def inner(grammar: Grammar): Unit = {

      if (closed.contains(grammar))
        return

      closed += grammar
      grammar.simplify match {
        case labelled: Labelled => inner(labelled.inner)
        case sequence: Sequence =>
          inner(sequence.first)
          inner(sequence.second)
        case choice: Choice =>
          inner(choice.left)
          inner(choice.right)
        case map: MapGrammar => inner(map.inner)
        case many: Many => inner(many.inner)
        case _ => Set.empty
      }
    }

    grammars.values.foreach(labelled => inner(labelled))
    closed
  }
}
case class GrammarNotFoundException(key: Any, inner: Exception) extends RuntimeException(inner)
