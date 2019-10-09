package deltas.yaml

import core.responsiveDocument.ResponsiveDocument

class WithIndentationGrammar(inner: BiGrammar) extends CustomGrammar {
  override def print(toDocumentInner: BiGrammar => ResponsiveDocument) = toDocumentInner(inner)

  override def createPrinter(recursive: BiGrammar => NodePrinter) = recursive(inner)

  override def toParser(recursive: BiGrammar => BiGrammarToParser.Self[Result]) = {
    BiGrammarToParser.WithIndentation(recursive(inner))
  }

  override def children = Seq(inner)

  override def withChildren(newChildren: Seq[BiGrammar]) = new WithIndentationGrammar(newChildren.head)

  override def containsParser(recursive: BiGrammar => Boolean) = true
}