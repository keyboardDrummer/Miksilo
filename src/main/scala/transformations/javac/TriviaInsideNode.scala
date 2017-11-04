package transformations.javac

import core.bigrammar._
import core.particles.{DeltaWithGrammar, Language, NodeGrammar}
import core.particles.grammars.GrammarCatalogue

import scala.collection.immutable.List

//noinspection ZeroIndexToHead
object TriviaInsideNode extends DeltaWithGrammar {

  override def description: String = "Moves trivia grammars left of a node to the inside of the node"

  override def transformGrammars(grammars: GrammarCatalogue, state: Language): Unit = {
    var visited = Set.empty[BiGrammar]
    for(path <- grammars.root.descendants)
    {
      if (!visited.contains(path.value)) {
        visited += path.value
        path.value match {
          case trivia: WithTrivia
            if hasLeftNode(trivia) =>
              path.set(trivia.getGrammar)
              injectTrivia(grammars, path, trivia.inner.isInstanceOf[Sequence])
          case _ =>
        }
      }
    }
  }

  private def hasLeftNode(path: GrammarPath) = {
    getLeftChildren(path).exists(p => p.value.isInstanceOf[NodeGrammar])
  }

  def injectTrivia(grammars: GrammarCatalogue, grammar: GrammarReference, horizontal: Boolean): Unit = {
    grammar.value match {
      case _:SequenceLike => injectTrivia(grammars, grammar.children.head, horizontal)
      case _:NodeGrammar => placeTrivia(grammars, grammar.children.head, horizontal)
      case _:Choice =>
        injectTrivia(grammars, grammar.children(0), horizontal)
        injectTrivia(grammars, grammar.children(1), horizontal)
      case _:WithTrivia =>
      case _:BiFailure =>
      case _ =>
        if (grammar.children.length == 1)
          injectTrivia(grammars, grammar.children.head, horizontal)
        else placeTrivia(grammars, grammar, horizontal)
    }
  }

  def placeTrivia(grammars: GrammarCatalogue, grammar: GrammarReference, horizontal: Boolean) = {
    if (!grammar.value.isInstanceOf[WithTrivia])
      grammar.set(new WithTrivia(grammar.value, grammars.trivia, horizontal))
  }

  def getLeftChildren(reference: GrammarPath): List[GrammarPath] = {
    val tail: List[GrammarPath] = reference.value match {
      case _: SequenceLike => getLeftChildren(reference.children.head)
      case _: Choice => getLeftChildren(reference.children(0)) ++ getLeftChildren(reference.children(1))
      case _ => reference.newChildren.flatMap(c => getLeftChildren(c))
    }
    reference :: tail
  }

}
