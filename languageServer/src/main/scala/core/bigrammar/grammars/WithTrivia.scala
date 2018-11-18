package core.bigrammar.grammars

import core.bigrammar.{BiGrammar, DefaultBiGrammarWriter}

object WithTrivia extends DefaultBiGrammarWriter {
  def withTrivia(grammar: BiGrammar, trivia: BiGrammar = ParseWhiteSpace, horizontal: Boolean = true): BiGrammar = {
    if (horizontal) trivia ~> grammar
    else trivia %> grammar
  }

  def getWithTrivia(grammar: BiGrammar, trivia: BiGrammar): Option[Sequence] = {
    grammar match {
      case sequence: Sequence if sequence.second == trivia => Some(sequence)
      case _ => None
    }
  }
}