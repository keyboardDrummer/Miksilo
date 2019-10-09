package core.bigrammar.grammars

import core.responsiveDocument.ResponsiveDocument

trait CustomGrammar extends BiGrammar {

  def print(toDocumentInner: BiGrammar => ResponsiveDocument): ResponsiveDocument
  def createPrinter(recursive: BiGrammar => NodePrinter): NodePrinter
  def toParser(recursive: BiGrammar => Self[Result]): Self[Result]
}
