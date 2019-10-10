package core.bigrammar.grammars

import core.bigrammar.BiGrammarToParser.Result
import core.bigrammar.printer.Printer.NodePrinter
import core.bigrammar.printer.{Printer, TryState}
import core.bigrammar.{BiGrammar, BiGrammarToParser, WithMap}
import core.document.{Empty, Text}
import core.responsiveDocument.ResponsiveDocument
import BiGrammarToParser._
import core.bigrammar.printer.BiGrammarToPrinter.ToPrinterCached

case class Parse[Value](grammar: BiGrammar[Value]) extends CustomGrammar[Value] {
  override def children: Seq[BiGrammar[_]] = Seq(grammar)

  override def withChildren(newChildren: Seq[BiGrammar[_]]): BiGrammar[Value] = Parse(newChildren.head.asInstanceOf[BiGrammar[Value]])

  override def containsParser(recursive: BiGrammar[_] => Boolean): Boolean = true

  override def print(toDocumentInner: BiGrammar[_] => ResponsiveDocument): ResponsiveDocument = Text("Parse") ~ toDocumentInner(grammar)

  override def createPrinter(recursive: ToPrinterCached): Printer[Value] = new Printer[Value]() {
    override def write(from: Value): TryState[ResponsiveDocument] = TryState.value(Empty)
  }

  override def toParser(recursive: Rec): Self[Value] = recursive(grammar)
}
