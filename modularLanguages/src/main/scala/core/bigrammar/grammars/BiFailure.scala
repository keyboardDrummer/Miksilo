package core.bigrammar.grammars

case class BiFailure(message: String = "") extends BiGrammarWithoutChildren {
  override def containsParser(recursive: BiGrammar => Boolean): Boolean = true
}
