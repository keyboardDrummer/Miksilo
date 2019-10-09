package deltas.yaml

import core.responsiveDocument.ResponsiveDocument
import deltas.yaml.YamlCoreDelta.WithContextParser

class WithContext[Result](update: YamlContext => YamlContext, inner: BiGrammar) extends CustomGrammar with BiGrammar {
  override def print(toDocumentInner: BiGrammar => ResponsiveDocument) = toDocumentInner(inner)

  override def createPrinter(recursive: BiGrammar => NodePrinter) = recursive(inner)

  override def toParser(recursive: BiGrammar => BiGrammarToParser.Self[BiGrammarToParser.Result]) =
    new WithContextParser(update, recursive(inner))

  override def children = Seq(inner)

  override def withChildren(newChildren: Seq[BiGrammar]) = new WithContext(update, newChildren.head)

  override def containsParser(recursive: BiGrammar => Boolean) = true
}
