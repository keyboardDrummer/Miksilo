package core.bigrammar.grammars

import core.bigrammar.BiGrammarToParser.AnyWithMap
import core.bigrammar.WithMap
import core.bigrammar.printer.TryState
import core.responsiveDocument.ResponsiveDocument

abstract class PrintUsingToStringGrammar(verifyWhenPrinting: Boolean = true)
  extends StringGrammar(verifyWhenPrinting) {

  override def write(from: AnyWithMap): TryState[ResponsiveDocument] =
    super.write(WithMap(from.value.toString, from.map))
}
