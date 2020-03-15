package miksilo.modularLanguages.deltas.yaml

import miksilo.modularLanguages.core.bigrammar.BiGrammarToParser.Result
import miksilo.modularLanguages.core.bigrammar.{BiGrammar, BiGrammarToParser}
import miksilo.modularLanguages.core.bigrammar.grammars.CustomGrammar
import miksilo.modularLanguages.core.bigrammar.printer.Printer.NodePrinter
import miksilo.editorParser.responsiveDocument.ResponsiveDocument

class WithIndentationGrammar(inner: BiGrammar) extends CustomGrammar {
  override def print(toDocumentInner: BiGrammar => ResponsiveDocument) = toDocumentInner(inner)

  override def createPrinter(recursive: BiGrammar => NodePrinter) = recursive(inner)

  override def toParser(recursive: BiGrammar => BiGrammarToParser.Parser[Result]) = {
    BiGrammarToParser.WithIndentation(recursive(inner))
  }

  override def children = Seq(inner)

  override def withChildren(newChildren: Seq[BiGrammar]) = new WithIndentationGrammar(newChildren.head)

  override def containsParser(recursive: BiGrammar => Boolean) = true
}