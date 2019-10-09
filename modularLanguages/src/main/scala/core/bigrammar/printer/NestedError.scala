package core.bigrammar.printer

import core.responsiveDocument.ResponsiveDocument

case class NestedError(value: Any, grammar: BiGrammar, inner: PrintError) extends PrintError {
  def partial = inner.partial

  val depth = inner.depth

  override def toDocument = inner.toDocument %
    ("Value:": ResponsiveDocument) ~~ value.toString %
    ("Grammar:": ResponsiveDocument) ~~ PrintBiGrammar.toDocument(grammar)
}
