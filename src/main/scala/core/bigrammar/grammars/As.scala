package core.bigrammar.grammars

import core.bigrammar.PrintBiGrammar.withParenthesis
import core.bigrammar.{BiGrammar, BiGrammarToParser, WithMapG}
import core.bigrammar.printer.AsPrinter
import core.bigrammar.printer.Printer.NodePrinter
import core.language.node.{NodeField, SourceRange}
import core.responsiveDocument.ResponsiveDocument

case class As(var inner: BiGrammar, field: NodeField) extends CustomGrammar
{
  override def children: Seq[BiGrammar] = Seq(inner)

  override def withChildren(newChildren: Seq[BiGrammar]) = As(newChildren.head, field)

  override def containsParser(recursive: BiGrammar => Boolean): Boolean = recursive(inner)

  override def print(toDocumentInner: BiGrammar => ResponsiveDocument): ResponsiveDocument = withParenthesis(inner) ~ s".As($field)"

  override def createPrinter(recursive: BiGrammar => NodePrinter): NodePrinter = new AsPrinter(recursive(inner), field)

  override def toParser(recursive: BiGrammar => Parser): Parser = {

    val innerParser = recursive(inner)
    for {
      start <- BiGrammarToParser.position
      inner <- innerParser
      end <- BiGrammarToParser.position
    } yield inner.map { case WithMapG(v, state) => WithMapG(inner, state +
      (field -> v) +
      (FieldPosition(field) -> SourceRange(start, end))) }
  }
}
