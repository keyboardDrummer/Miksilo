package miksilo.modularLanguages.core.bigrammar.grammars

import miksilo.modularLanguages.core.bigrammar.BiGrammar
import miksilo.modularLanguages.core.bigrammar.BiGrammarToParser._
import miksilo.modularLanguages.core.bigrammar.printer.Printer.NodePrinter
import miksilo.editorParser.responsiveDocument.ResponsiveDocument

trait CustomGrammar extends BiGrammar {

  def print(toDocumentInner: BiGrammar => ResponsiveDocument): ResponsiveDocument
  def createPrinter(recursive: BiGrammar => NodePrinter): NodePrinter
  def toParser(recursive: BiGrammar => Parser[Result]): Parser[Result]
}
