package miksilo.modularLanguages.core.bigrammar.printer

import miksilo.modularLanguages.core.bigrammar.{BiGrammar, PrintBiGrammar}
import miksilo.editorParser.responsiveDocument.ResponsiveDocument

case class NestedError(value: Any, grammar: BiGrammar, inner: PrintError) extends PrintError {
  def partial = inner.partial

  val depth = inner.depth

  override def toDocument = inner.toDocument %
    ("Value:": ResponsiveDocument) ~~ value.toString %
    ("Grammar:": ResponsiveDocument) ~~ PrintBiGrammar.toDocument(grammar)
}
