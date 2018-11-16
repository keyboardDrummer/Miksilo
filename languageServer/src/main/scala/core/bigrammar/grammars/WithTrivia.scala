package core.bigrammar.grammars

import core.bigrammar.BiGrammar

object WithTrivia {
  def withTrivia(grammar: BiGrammar, trivia: BiGrammar = ParseWhiteSpace, horizontal: Boolean = true): BiGrammar = {
    if (horizontal) new LeftRight(trivia, grammar, Sequence.ignoreLeft)
    else new TopBottom(trivia, grammar, Sequence.ignoreLeft)
  }

  def getWithTrivia(grammar: BiGrammar, trivia: BiGrammar): Option[Sequence] = {
    grammar match {
      case sequence: Sequence if sequence.second == trivia => Some(sequence)
      case _ => None
    }
  }
}