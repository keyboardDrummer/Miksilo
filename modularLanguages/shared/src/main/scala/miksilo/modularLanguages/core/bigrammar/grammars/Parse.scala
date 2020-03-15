package miksilo.modularLanguages.core.bigrammar.grammars

import miksilo.modularLanguages.core.bigrammar.{BiGrammar, WithMap}
import miksilo.editorParser.document.{Empty, Text}
import miksilo.modularLanguages.core.bigrammar.BiGrammarToParser._
import miksilo.modularLanguages.core.bigrammar.printer.Printer.NodePrinter
import miksilo.modularLanguages.core.bigrammar.printer.TryState
import miksilo.editorParser.responsiveDocument.ResponsiveDocument

case class Parse(grammar: BiGrammar) extends CustomGrammar {
  override def children: Seq[BiGrammar] = Seq(grammar)

  override def withChildren(newChildren: Seq[BiGrammar]): BiGrammar = Parse(newChildren.head)

  override def containsParser(recursive: BiGrammar => Boolean): Boolean = true

  override def print(toDocumentInner: BiGrammar => ResponsiveDocument): ResponsiveDocument = Text("Parse") ~ toDocumentInner(grammar)

  override def createPrinter(recursive: BiGrammar => NodePrinter): NodePrinter = new NodePrinter() {
    override def write(from: WithMap[Any]): TryState[ResponsiveDocument] = TryState.value(Empty)
  }

  override def toParser(recursive: BiGrammar => Parser[Result]): Parser[Result] = recursive(grammar)
}
