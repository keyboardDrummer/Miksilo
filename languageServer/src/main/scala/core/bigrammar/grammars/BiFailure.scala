package core.bigrammar.grammars

import core.bigrammar.BiGrammar

case class BiFailure[Value](message: String = "") extends BiGrammarWithoutChildren[Value] {
  override def containsParser(recursive: BiGrammar[_] => Boolean): Boolean = true
}
