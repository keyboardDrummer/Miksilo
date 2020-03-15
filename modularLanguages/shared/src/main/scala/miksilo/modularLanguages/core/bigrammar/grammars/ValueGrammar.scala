package miksilo.modularLanguages.core.bigrammar.grammars

import miksilo.modularLanguages.core.bigrammar.BiGrammar

/**
  * Does not consume or produce any syntax, but simply produces or consumes a value.
  */
case class ValueGrammar(value: Any) extends BiGrammarWithoutChildren {
  override def containsParser(recursive: BiGrammar => Boolean): Boolean = false
}
