package miksilo.modularLanguages.core.bigrammar.grammars

import miksilo.editorParser.parsers.core.TextPointer
import miksilo.modularLanguages.core.bigrammar.BiGrammarToParser._
import miksilo.modularLanguages.core.bigrammar.printer.AsPrinter
import miksilo.modularLanguages.core.bigrammar.printer.Printer.NodePrinter
import miksilo.modularLanguages.core.bigrammar.{BiGrammar, PrintBiGrammar, WithMap}
import miksilo.modularLanguages.core.node.NodeField
import miksilo.editorParser.parsers.editorParsers.OffsetPointerRange
import miksilo.editorParser.responsiveDocument.ResponsiveDocument

case class As(var inner: BiGrammar, field: NodeField, changePosition: (TextPointer, TextPointer) => OffsetPointerRange = null) extends CustomGrammar
{
  override def children: Seq[BiGrammar] = Seq(inner)

  override def withChildren(newChildren: Seq[BiGrammar]) = As(newChildren.head, field, changePosition)

  override def containsParser(recursive: BiGrammar => Boolean): Boolean = recursive(inner)

  override def print(toDocumentInner: BiGrammar => ResponsiveDocument): ResponsiveDocument = PrintBiGrammar.withParenthesis(inner) ~ s".As($field)"

  override def createPrinter(recursive: BiGrammar => NodePrinter): NodePrinter = new AsPrinter(recursive(inner), field)

  override def toParser(recursive: BiGrammar => Parser[Result]): Parser[Result] = {
    recursive(inner).withRange[Result]((from, until, result: Result) => {
      val range =
        if (changePosition == null) OffsetPointerRange(from, until)
        else changePosition(from, until)
      WithMap[Any]((), result.namedValues + (field -> result.value) + (FieldPosition(field) -> range))
    })
  }
}
