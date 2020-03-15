package miksilo.modularLanguages.core.bigrammar.grammars

import miksilo.modularLanguages.core.bigrammar.BiGrammar
import miksilo.editorParser.responsiveDocument.ResponsiveDocument

/**
  * Prints a value, but parses nothing.
  */
case class Print(document: ResponsiveDocument) extends BiGrammarWithoutChildren {
  override def containsParser(recursive: BiGrammar => Boolean): Boolean = false
}