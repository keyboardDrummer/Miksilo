package core.bigrammar.grammars

import core.bigrammar.BiGrammar.State
import core.bigrammar.BiGrammarToParser.AnyWithMap
import core.bigrammar.WithMap
import core.bigrammar.printer.Printer.TryState
import core.responsiveDocument.ResponsiveDocument

abstract class PrintUsingToStringGrammar(verifyWhenPrinting: Boolean = true)
  extends StringGrammar(verifyWhenPrinting) {

  override def write(from: AnyWithMap, state: State): TryState[ResponsiveDocument] =
    super.write(WithMap(from.value.toString, from.namedValues), state)
}
