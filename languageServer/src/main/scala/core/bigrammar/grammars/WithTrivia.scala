package core.bigrammar.grammars

import core.bigrammar.{BiGrammar, DefaultBiGrammarWriter}
import core.language.node.GrammarKey

object WithTrivia extends DefaultBiGrammarWriter {
  def withTrivia(grammar: BiGrammar, trivia: BiGrammar = ParseWhiteSpace, horizontal: Boolean = true): BiGrammar = {
    new WithTrivia(grammar, trivia, horizontal)
//    if (horizontal) trivia ~> grammar
//    else trivia %> grammar
  }

  def getWithTrivia(grammar: BiGrammar): Option[WithTrivia] = {
    grammar match {
      case sequence: WithTrivia => Some(sequence)
      case _ => None
    }
  }
}

object WithTriviaKey extends GrammarKey
class WithTrivia(grammar: BiGrammar, trivia: BiGrammar = ParseWhiteSpace, val horizontal: Boolean = true)
  extends Labelled(WithTriviaKey, if (horizontal) new LeftRight(trivia, grammar, Sequence.ignoreLeft) else new TopBottom(trivia, grammar, Sequence.ignoreLeft)) {
  def getGrammar = grammar

  override def containsParser(recursive: BiGrammar => Boolean): Boolean = true

  override protected def getLeftChildren(recursive: BiGrammar => Seq[BiGrammar]) = recursive(grammar) //TODO maybe if we can remove all the WithTrivia's first in TriviaInsideNode we wouldn't need this hack.
}