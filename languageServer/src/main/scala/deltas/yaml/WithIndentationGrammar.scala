package deltas.yaml

import core.bigrammar.BiGrammarToParser.Rec
import core.bigrammar.grammars.CustomGrammar
import core.bigrammar.printer.BiGrammarToPrinter
import core.bigrammar.{BiGrammar, BiGrammarToParser}
import core.responsiveDocument.ResponsiveDocument

class WithIndentationGrammar[Value](inner: BiGrammar[Value]) extends CustomGrammar[Value] {
  override def print(toDocumentInner: BiGrammar[_] => ResponsiveDocument) = toDocumentInner(inner)

  override def createPrinter(recursive: BiGrammarToPrinter.ToPrinterCached) = recursive(inner)

  override def toParser(recursive: Rec) = {
    BiGrammarToParser.WithIndentation(recursive(inner))
  }

  override def children = Seq(inner)

  override def withChildren(newChildren: Seq[BiGrammar[_]]) = new WithIndentationGrammar(newChildren.head.asInstanceOf[BiGrammar[Value]])

  override def containsParser(recursive: BiGrammar[_] => Boolean) = true
}