package core.bigrammar.grammars

import core.bigrammar.{BiGrammar, BiGrammarToParser}
import core.bigrammar.BiGrammarToParser.Result
import core.bigrammar.printer.Printer.NodePrinter
import core.parsers.strings.StringParserWriter
import core.responsiveDocument.ResponsiveDocument

trait CustomGrammar extends BiGrammar with StringParserWriter {

  def print(toDocumentInner: BiGrammar => ResponsiveDocument): ResponsiveDocument
  def createPrinter(recursive: BiGrammar => NodePrinter): NodePrinter
  def toParser(recursive: BiGrammar => BiGrammarToParser.EditorParser[Result]): BiGrammarToParser.EditorParser[Result]
}
