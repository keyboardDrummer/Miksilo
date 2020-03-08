package core.bigrammar.grammars

import core.bigrammar.BiGrammarToParser._
import core.bigrammar.printer.AsPrinter
import core.bigrammar.printer.Printer.NodePrinter
import core.bigrammar.{BiGrammar, PrintBiGrammar, WithMap}
import core.language.node.NodeField
import core.parsers.core.TextPointer
import core.parsers.editorParsers.OffsetNodeRange
import core.responsiveDocument.ResponsiveDocument

case class As(var inner: BiGrammar, field: NodeField, changePosition: (TextPointer, TextPointer) => OffsetNodeRange = null) extends CustomGrammar
{
  override def children: Seq[BiGrammar] = Seq(inner)

  override def withChildren(newChildren: Seq[BiGrammar]) = As(newChildren.head, field, changePosition)

  override def containsParser(recursive: BiGrammar => Boolean): Boolean = recursive(inner)

  override def print(toDocumentInner: BiGrammar => ResponsiveDocument): ResponsiveDocument = PrintBiGrammar.withParenthesis(inner) ~ s".As($field)"

  override def createPrinter(recursive: BiGrammar => NodePrinter): NodePrinter = new AsPrinter(recursive(inner), field)

  override def toParser(recursive: BiGrammar => Parser[Result]): Parser[Result] = {
    recursive(inner).withRange[Result]((from, until, result: Result) => {
      val range =
        if (changePosition == null) OffsetNodeRange(from, until)
        else changePosition(from, until)
      WithMap[Any]((), result.namedValues + (field -> result.value) + (FieldPosition(field) -> range))
    })
  }
}
