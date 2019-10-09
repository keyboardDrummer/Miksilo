package core.bigrammar.grammars

import core.responsiveDocument.ResponsiveDocument

/**
  * Prints a value, but parses nothing.
  */
case class Print(document: ResponsiveDocument) extends BiGrammarWithoutChildren {
  override def containsParser(recursive: BiGrammar => Boolean): Boolean = false
}