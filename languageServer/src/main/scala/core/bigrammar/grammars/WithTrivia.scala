package core.bigrammar.grammars

import core.bigrammar.BiGrammar

class WithTrivia(inner: BiGrammar, trivia: BiGrammar = ParseWhiteSpace, horizontal: Boolean = true)
  extends BiSequence(trivia, inner, BiSequence.ignoreLeft, horizontal) {
  def getGrammar: BiGrammar = second

  override def containsParser(recursive: BiGrammar => Boolean): Boolean = true

  override protected def getLeftChildren(recursive: BiGrammar => Seq[BiGrammar]) = recursive(inner) //TODO maybe if we can remove all the WithTrivia's first in TriviaInsideNode we wouldn't need this hack.
}