package core.bigrammar.grammars

import core.bigrammar.BiGrammar
import core.responsiveDocument.ResponsiveDocument

/**
  * Prints a value, but parses nothing.
  */
case class Print(document: ResponsiveDocument) extends BiGrammarWithoutChildren[Unit] {
  override def containsParser(recursive: BiGrammar[_] => Boolean): Boolean = false
}