package core.bigrammar.grammars

import core.bigrammar.BiGrammarToParser.Result
import core.bigrammar.printer.Printer.NodePrinter
import core.bigrammar.{BiGrammar, BiGrammarToParser}
import core.responsiveDocument.ResponsiveDocument
import BiGrammarToParser._

class WithDefault(inner: BiGrammar, _default: Any) extends CustomGrammar {
  override def print(toDocumentInner: BiGrammar => ResponsiveDocument): ResponsiveDocument = toDocumentInner(inner) ~ "withDefault: " ~ _default.toString

  override def createPrinter(recursive: BiGrammar => NodePrinter): NodePrinter = recursive(inner)

  override def toParser(recursive: BiGrammar => EditorParser[Result]): EditorParser[Result] =
    recursive(inner).withDefault[Result](valueToResult(_default))

  override def children: Seq[BiGrammar] = Seq(inner)

  override def withChildren(newChildren: Seq[BiGrammar]): BiGrammar = new core.bigrammar.grammars.WithDefault(newChildren.head, _default)

  override def containsParser(recursive: BiGrammar => Boolean): Boolean = recursive(inner)
}
