package core.bigrammar.grammars

import core.bigrammar.{BiGrammar, BiGrammarToParser}
import core.bigrammar.BiGrammarToParser._
import core.bigrammar.printer.BiGrammarToPrinter.ToPrinterCached
import core.bigrammar.printer.Printer.NodePrinter
import core.responsiveDocument.ResponsiveDocument

case class Colorize[Value](var inner: BiGrammar[Value], textMateScope: String) extends CustomGrammar[Value] {

  override def children = Seq(inner)

  override def withChildren(newChildren: Seq[BiGrammar[_]]) =
    Colorize(newChildren.head.asInstanceOf[BiGrammar[Value]], textMateScope)

  override def containsParser(recursive: BiGrammar[_] => Boolean): Boolean = inner.containsParser(recursive)

  override def print(toDocumentInner: BiGrammar[_] => ResponsiveDocument) = toDocumentInner(inner)

  override def createPrinter(recursive: ToPrinterCached) = recursive(inner)

  override def toParser(recursive: Rec) = recursive(inner)
}
