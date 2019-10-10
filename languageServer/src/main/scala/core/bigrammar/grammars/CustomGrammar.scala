package core.bigrammar.grammars

import core.bigrammar.BiGrammar
import core.bigrammar.BiGrammarToParser._
import core.bigrammar.printer.{BiGrammarToPrinter, Printer}
import core.responsiveDocument.ResponsiveDocument

trait CustomGrammar[Value] extends BiGrammar[Value] {

  def print(toDocumentInner: BiGrammar[_] => ResponsiveDocument): ResponsiveDocument
  def createPrinter(recursive: BiGrammarToPrinter.ToPrinterCached): Printer[Value]
  def toParser(recursive: Rec): Self[Value]
}
