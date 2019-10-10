package core.bigrammar.grammars

import core.bigrammar.BiGrammar

/**
  * Does not consume or produce any syntax, but simply produces or consumes a value.
  */
case class ValueGrammar[Value](value: Value) extends BiGrammarWithoutChildren[Value] {
  override def containsParser(recursive: BiGrammar[_] => Boolean): Boolean = false
}
