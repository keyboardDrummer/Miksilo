package core.bigrammar.grammars

import core.bigrammar.BiGrammar

class WithTrivia(grammar: BiGrammar, trivia: BiGrammar = ParseWhiteSpace, horizontal: Boolean = true)
  extends IgnoreLeft(if (horizontal) new LeftRight(trivia, grammar) else new TopBottom(trivia, grammar)) {
  def getGrammar = sequence.second

  override def containsParser(recursive: BiGrammar => Boolean): Boolean = true

  override protected def getLeftChildren(recursive: BiGrammar => Seq[BiGrammar]) = recursive(grammar) //TODO maybe if we can remove all the WithTrivia's first in TriviaInsideNode we wouldn't need this hack.
}
