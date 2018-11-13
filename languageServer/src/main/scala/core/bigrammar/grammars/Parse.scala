package core.bigrammar.grammars

import core.bigrammar.BiGrammarToParser.Result
import core.bigrammar.printer.Printer.NodePrinter
import core.bigrammar.printer.TryState
import core.bigrammar.{BiGrammar, WithMap}
import core.document.{Empty, Text}
import core.responsiveDocument.ResponsiveDocument

case class Parse(grammar: BiGrammar) extends CustomGrammar {
  override def children: Seq[BiGrammar] = Seq(grammar)

  override def withChildren(newChildren: Seq[BiGrammar]): BiGrammar = Parse(newChildren.head)

  override def containsParser(recursive: BiGrammar => Boolean): Boolean = true

  override def print(toDocumentInner: BiGrammar => ResponsiveDocument): ResponsiveDocument = Text("Parse") ~ toDocumentInner(grammar)

  override def createPrinter(recursive: BiGrammar => NodePrinter): NodePrinter = new NodePrinter() {
    override def write(from: WithMap[Any]): TryState[ResponsiveDocument] = TryState.value(Empty)
  }

  override def toParser(recursive: BiGrammar => Parser[Result]): Parser[Result] = recursive(grammar)
}
