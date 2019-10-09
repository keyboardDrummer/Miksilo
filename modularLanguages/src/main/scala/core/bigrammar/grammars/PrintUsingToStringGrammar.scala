package core.bigrammar.grammars

import core.responsiveDocument.ResponsiveDocument

abstract class PrintUsingToStringGrammar(verifyWhenPrinting: Boolean = true)
  extends StringGrammar(verifyWhenPrinting) {

  override def write(from: AnyWithMap): TryState[ResponsiveDocument] =
    super.write(WithMap(from.value.toString, from.namedValues))
}
