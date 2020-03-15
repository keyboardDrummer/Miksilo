package miksilo.modularLanguages.deltas.yaml

import miksilo.modularLanguages.core.bigrammar.{BiGrammar, BiGrammarToParser}
import miksilo.modularLanguages.core.bigrammar.grammars.CustomGrammar
import miksilo.modularLanguages.core.bigrammar.printer.Printer.NodePrinter
import miksilo.editorParser.responsiveDocument.ResponsiveDocument
import miksilo.modularLanguages.deltas.yaml.YamlCoreDelta.WithContextParser

class WithContext[Result](update: YamlContext => YamlContext, inner: BiGrammar) extends CustomGrammar with BiGrammar {
  override def print(toDocumentInner: BiGrammar => ResponsiveDocument) = toDocumentInner(inner)

  override def createPrinter(recursive: BiGrammar => NodePrinter) = recursive(inner)

  override def toParser(recursive: BiGrammar => BiGrammarToParser.Parser[BiGrammarToParser.Result]) =
    new WithContextParser(update, recursive(inner))

  override def children = Seq(inner)

  override def withChildren(newChildren: Seq[BiGrammar]) = new WithContext(update, newChildren.head)

  override def containsParser(recursive: BiGrammar => Boolean) = true
}
