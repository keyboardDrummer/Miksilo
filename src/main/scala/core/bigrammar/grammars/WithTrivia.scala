package core.bigrammar.grammars

import core.bigrammar.BiGrammar

class WithTrivia(grammar: BiGrammar, trivia: BiGrammar = ParseWhiteSpace, horizontal: Boolean = true)
  extends IgnoreLeft(if (horizontal) new LeftRight(trivia, grammar) else new TopBottom(trivia, grammar)) {
  def getGrammar = sequence.second

  override def containsParser(recursive: BiGrammar => Boolean): Boolean = true
}
