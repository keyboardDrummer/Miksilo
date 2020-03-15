package miksilo.modularLanguages.core.bigrammar.grammars

import miksilo.modularLanguages.core.bigrammar.BiGrammarToParser.AnyWithMap
import miksilo.modularLanguages.core.bigrammar.WithMap
import miksilo.modularLanguages.core.bigrammar.printer.TryState
import miksilo.editorParser.responsiveDocument.ResponsiveDocument

abstract class PrintUsingToStringGrammar(verifyWhenPrinting: Boolean = true)
  extends StringGrammar(verifyWhenPrinting) {

  override def write(from: AnyWithMap): TryState[ResponsiveDocument] =
    super.write(WithMap(from.value.toString, from.namedValues))
}
