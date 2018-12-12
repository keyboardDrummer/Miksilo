package core.bigrammar.grammars

import core.bigrammar.BiGrammar
import core.bigrammar.BiGrammarToParser._
import core.bigrammar.printer.Printer.NodePrinter
import core.responsiveDocument.ResponsiveDocument

trait CustomGrammar extends BiGrammar {

  def print(toDocumentInner: BiGrammar => ResponsiveDocument): ResponsiveDocument
  def createPrinter(recursive: BiGrammar => NodePrinter): NodePrinter
  def toParser(recursive: BiGrammar => EditorParser[Result]): EditorParser[Result]
}
