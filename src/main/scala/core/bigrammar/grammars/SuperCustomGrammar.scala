package core.bigrammar.grammars

import core.bigrammar.BiGrammar
import core.bigrammar.printer.TryState.NodePrinter
import core.grammar.Grammar
import core.responsiveDocument.ResponsiveDocument

trait SuperCustomGrammar extends BiGrammar {
  def print(toDocumentInner: (BiGrammar) => ResponsiveDocument): ResponsiveDocument
  def createGrammar(children: Seq[Grammar], recursive: (BiGrammar) => Grammar): Grammar
  def createPrinter(recursive: BiGrammar => NodePrinter): NodePrinter
}
