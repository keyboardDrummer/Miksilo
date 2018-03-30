package core.bigrammar.grammars

import core.bigrammar.BiGrammar
import core.bigrammar.printer.Printer.NodePrinter
import core.responsiveDocument.ResponsiveDocument

case class Colorize(var inner: BiGrammar, _type: Int) extends CustomGrammar {

  override def children = Seq(inner)

  override def withChildren(newChildren: Seq[BiGrammar]) = Colorize(newChildren.head, _type)

  override def containsParser(recursive: BiGrammar => Boolean): Boolean = inner.containsParser(recursive)

  override def print(toDocumentInner: BiGrammar => ResponsiveDocument) = toDocumentInner(inner)

  override def createPrinter(recursive: BiGrammar => NodePrinter) = recursive(inner)

  override def toParser(recursive: BiGrammar => Parser) = recursive(inner)
}
