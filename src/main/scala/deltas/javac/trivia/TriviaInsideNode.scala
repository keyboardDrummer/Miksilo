package deltas.javac.trivia

import core.bigrammar._
import core.bigrammar.grammars._
import core.deltas.grammars.LanguageGrammars
import core.deltas.{DeltaWithGrammar, Language, NodeGrammar}

//noinspection ZeroIndexToHead
object TriviaInsideNode extends DeltaWithGrammar {

  override def description: String = "Moves trivia grammars left of a node to the inside of the node"

  override def transformGrammars(grammars: LanguageGrammars, language: Language): Unit = {
    var visited = Set.empty[BiGrammar]
    val descendants = grammars.root.descendants.sortBy(ref => ref.toString())
    for(path <- descendants)
    {
      path.value match {
        case trivia: WithTrivia =>
          if (!visited.contains(path.value)) {
            visited += path.value

            val grammar = trivia.getGrammar
            if (hasLeftNode(new RootGrammar(grammar))) {
              path.set(trivia.getGrammar)
              injectTrivia(grammars, path, trivia.inner.isInstanceOf[LeftRight])
            }
          }
        case _ =>
      }
    }
  }

  private def hasLeftNode(path: GrammarPath) = {
    val leftChildren = path.value.getLeftChildren
    leftChildren.exists(p => p.isInstanceOf[NodeGrammar])
  }

  def injectTrivia(grammars: LanguageGrammars, grammar: GrammarReference, horizontal: Boolean): Unit = {
    grammar.value match {
      case sequence: Sequence =>
        val left = sequence.getLeftChildren.drop(1).head
        val child = grammar.children.find(ref => ref.value == left).get
        injectTrivia(grammars, child, horizontal)
      case _:NodeGrammar =>
        if (!isLeftRecursive(grammar.children.head)) {
          placeTrivia(grammars, grammar.children.head, horizontal)
        }
      case _:Choice =>
        injectTrivia(grammars, grammar.children(0), horizontal)
        injectTrivia(grammars, grammar.children(1), horizontal)
      case _:WithTrivia => //TODO if we consider the grammars as a graph and only move WithTrivia's from all incoming edges at once, then we wouldn't need this hack.
      case _:BiFailure =>
      case _ =>
        if (grammar.children.length == 1)
          injectTrivia(grammars, grammar.children.head, horizontal)
        else placeTrivia(grammars, grammar, horizontal)
    }
  }

  def placeTrivia(grammars: LanguageGrammars, grammar: GrammarReference, horizontal: Boolean): Unit = {
    if (!grammar.value.isInstanceOf[WithTrivia] && grammar.value.containsParser()) {
      grammar.set(new WithTrivia(grammar.value, grammars.trivia, horizontal))
    }
  }

  def isLeftRecursive(grammar: GrammarPath): Boolean = {
    val edges = grammar.ancestors.collect({ case ref: GrammarReference => ref }).map(p => (p.property, p.parent)).toList
    if (edges.distinct.size != edges.size)
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
}
