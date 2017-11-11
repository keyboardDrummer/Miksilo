package core.bigrammar.grammars

import core.bigrammar.BiGrammar

case class Keyword(value: String, reserved: Boolean = true, verifyWhenPrinting: Boolean = false) extends BiGrammarWithoutChildren {
  override def containsParser(recursive: BiGrammar => Boolean): Boolean = true
}
