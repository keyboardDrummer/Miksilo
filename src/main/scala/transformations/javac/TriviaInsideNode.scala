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
            if hasLeftNode(trivia.getGrammar) =>
              path.set(trivia.getGrammar)
              injectTrivia(grammars, path, trivia.inner.isInstanceOf[Sequence])
          case _ =>
        }
      }
    }
    System.out.append("")
  }

  private def hasLeftNode(path: GrammarPath) = {
    getLeftChildren(path).exists(p => p.value.isInstanceOf[NodeGrammar])
  }

  def injectTrivia(grammars: GrammarCatalogue, grammar: GrammarReference, horizontal: Boolean): Unit = {
    grammar.value match {
      case _:SequenceLike =>
      case sequence: SequenceLike =>
        if (sequence.first.containsParser())
          injectTrivia(grammars, grammar.children.head, horizontal)
        else
          injectTrivia(grammars, grammar.children(1), horizontal)
      case _:NodeGrammar => if (!isLeftRecursive(grammar.children.head))
        placeTrivia(grammars, grammar.children.head, horizontal)
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

  def isLeftRecursive(grammar: GrammarPath): Boolean = {
    if (grammar.ancestorGrammars.size != grammar.ancestors.size + 1)
      return true

    grammar.value match {
      case _:SequenceLike => isLeftRecursive(grammar.children.head)
      case _:NodeGrammar =>
        false
      case _:Choice => isLeftRecursive(grammar.children(0)) || isLeftRecursive(grammar.children(1))
      case _:BiFailure =>
        false
      case _ =>
        if (grammar.children.length == 1)
          isLeftRecursive(grammar.children.head)
        else
          false
    }
  }

  def getLeftChildren(reference: GrammarPath): List[GrammarPath] = {
    val tail: List[GrammarPath] = reference.value match {
      case sequence: SequenceLike =>
        if (sequence.first.containsParser())
          getLeftChildren(reference.children.head)
        else
          getLeftChildren(reference.children(1))
      case _: Choice => getLeftChildren(reference.children(0)) ++ getLeftChildren(reference.children(1))
      case _ => reference.newChildren.flatMap(c => getLeftChildren(c))
    }
    reference :: tail
  }

}
