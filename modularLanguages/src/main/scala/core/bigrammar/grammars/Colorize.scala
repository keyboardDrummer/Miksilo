package core.bigrammar.grammars

import core.bigrammar.{BiGrammar, BiGrammarToParser}
import core.responsiveDocument.ResponsiveDocument
import BiGrammarToParser._
import core.bigrammar.printer.Printer.NodePrinter

case class Colorize(var inner: BiGrammar, textMateScope: String) extends CustomGrammar {

  override def children = Seq(inner)

  override def withChildren(newChildren: Seq[BiGrammar]) = Colorize(newChildren.head, textMateScope)

  override def containsParser(recursive: BiGrammar => Boolean): Boolean = inner.containsParser(recursive)

  override def print(toDocumentInner: BiGrammar => ResponsiveDocument) = toDocumentInner(inner)

  override def createPrinter(recursive: BiGrammar => NodePrinter) = recursive(inner)

  override def toParser(recursive: BiGrammar => Self[Result]) = recursive(inner)
}
