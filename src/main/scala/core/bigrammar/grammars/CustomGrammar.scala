package core.bigrammar.grammars

import core.bigrammar.BiGrammarToParser.Result
import core.bigrammar.printer.Printer.NodePrinter
import core.bigrammar.{BiGrammar, BiGrammarToParser}
import core.grammar.Grammar
import core.responsiveDocument.ResponsiveDocument

trait CustomGrammar extends BiGrammar {
  type Parser = BiGrammarToParser.Parser[Result]

  def print(toDocumentInner: (BiGrammar) => ResponsiveDocument): ResponsiveDocument
  def createGrammar(children: Seq[Grammar], recursive: (BiGrammar) => Grammar): Grammar
  def createPrinter(recursive: BiGrammar => NodePrinter): NodePrinter
  def toParser(recursive: BiGrammar => Parser): Parser
}
