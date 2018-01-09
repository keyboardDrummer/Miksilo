package deltas.javac.trivia

import core.bigrammar._
import core.bigrammar.grammars._
import core.deltas.grammars.LanguageGrammars
import core.deltas.{DeltaWithGrammar, Language, NodeGrammar}

import scala.collection.immutable.List

//noinspection ZeroIndexToHead
object TriviaInsideNode extends DeltaWithGrammar {

  override def description: String = "Moves trivia grammars left of a node to the inside of the node"

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    var visited = Set.empty[BiGrammar]
    val descendants = grammars.root.descendants
    System.out.println("descendants = " + descendants.toString())
    for(path <- descendants)
    {
      if (!visited.contains(path.value)) {
        visited += path.value
        path.value match {
          case trivia: WithTrivia
            if hasLeftNode(trivia.getGrammar) =>
              System.out.println("moving trivia in " + trivia.toString())
              path.set(trivia.getGrammar)
              injectTrivia(grammars, path, trivia.inner.isInstanceOf[LeftRight])
          case _ =>
        }
      }
    }
  }

  private def hasLeftNode(path: GrammarPath) = {
    getLeftChildren(path).exists(p => p.value.isInstanceOf[NodeGrammar])
  }

  def injectTrivia(grammars: LanguageGrammars, grammar: GrammarReference, horizontal: Boolean): Unit = {
    System.out.println("inject trivia called for " + grammar.value.toString())
    grammar.value match {
      case sequence: Sequence =>
        System.out.println("Sequence")
        if (sequence.first.containsParser())
          injectTrivia(grammars, grammar.children.head, horizontal)
        else
          injectTrivia(grammars, grammar.children(1), horizontal)
      case _:NodeGrammar => if (!isLeftRecursive(grammar.children.head))
        System.out.println("NodeGrammar")
        placeTrivia(grammars, grammar.children.head, horizontal)
      case _:Choice =>
        System.out.println("Choice")
        injectTrivia(grammars, grammar.children(0), horizontal)
        injectTrivia(grammars, grammar.children(1), horizontal)
      case _:WithTrivia => //TODO if we consider the grammars as a graph and only move WithTrivia's from all incoming edges at once, then we wouldn't need this hack.
        System.out.println("WithTrivia")
      case _:BiFailure =>
        System.out.println("BiFailure")
      case _ =>
        System.out.println("fallback")
        if (grammar.children.length == 1)
          injectTrivia(grammars, grammar.children.head, horizontal)
        else placeTrivia(grammars, grammar, horizontal)
    }
  }

  def placeTrivia(grammars: LanguageGrammars, grammar: GrammarReference, horizontal: Boolean): Unit = {
    System.out.println("placing trivia in " + grammar.value.toString)
    if (!grammar.value.isInstanceOf[WithTrivia] && grammar.value.containsParser())
      grammar.set(new WithTrivia(grammar.value, grammars.trivia, horizontal))
  }

  def isLeftRecursive(grammar: GrammarPath): Boolean = {
    if (grammar.ancestorGrammars.size != grammar.ancestors.size)
      return true

    grammar.value match {
      case _:Sequence => isLeftRecursive(grammar.children.head)
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
      case _: WithTrivia => getLeftChildren(reference.children.head.children(1)) //TODO maybe if we can remove all the WithTrivia's first we wouldn't need this hack.
      case sequence: Sequence =>
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
