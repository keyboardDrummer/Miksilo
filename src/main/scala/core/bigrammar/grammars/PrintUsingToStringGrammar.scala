package core.bigrammar.grammars

import core.bigrammar.BiGrammarToParser.WithMap
import core.bigrammar.WithMapG
import core.bigrammar.printer.TryState
import core.responsiveDocument.ResponsiveDocument

abstract class PrintUsingToStringGrammar(verifyWhenPrinting: Boolean = true)
  extends StringGrammar(verifyWhenPrinting) {

  override def write(from: WithMap): TryState[ResponsiveDocument] =
    super.write(WithMapG(from.value.toString, from.map))
}
