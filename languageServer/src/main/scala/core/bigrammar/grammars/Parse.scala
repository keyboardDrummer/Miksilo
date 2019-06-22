package core.bigrammar.grammars

import core.bigrammar.BiGrammarToParser.Result
import core.bigrammar.printer.Printer.{NodePrinter, TryState}
import core.bigrammar.{BiGrammar, BiGrammarToParser, WithMap}
import core.document.{Empty, Text}
import core.responsiveDocument.ResponsiveDocument
import BiGrammarToParser._
import core.bigrammar.BiGrammar.State

import scala.util.Try

case class Parse(grammar: BiGrammar) extends CustomGrammar {
  override def children: Seq[BiGrammar] = Seq(grammar)

  override def withChildren(newChildren: Seq[BiGrammar]): BiGrammar = Parse(newChildren.head)

  override def containsParser(recursive: BiGrammar => Boolean): Boolean = true

  override def print(toDocumentInner: BiGrammar => ResponsiveDocument): ResponsiveDocument = Text("Parse") ~ toDocumentInner(grammar)

  override def createPrinter(recursive: BiGrammar => NodePrinter): NodePrinter = new NodePrinter() {
    override def write(from: WithMap[Any], state: State): TryState[ResponsiveDocument] = scala.util.Success(state -> Empty)
  }

  override def toParser(recursive: BiGrammar => Self[Result]): Self[Result] = recursive(grammar)
}
