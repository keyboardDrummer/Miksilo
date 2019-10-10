package core.bigrammar.grammars

import core.bigrammar.BiGrammarToParser._
import core.bigrammar.PrintBiGrammar.withParenthesis
import core.bigrammar.printer.BiGrammarToPrinter.ToPrinterCached
import core.bigrammar.printer.{AsPrinter, Printer}
import core.bigrammar.printer.Printer.NodePrinter
import core.bigrammar.{BiGrammar, WithMap}
import core.language.node.NodeField
import core.responsiveDocument.ResponsiveDocument
import languageServer.SourceRange

case class As[Value](var inner: BiGrammar[Value], field: NodeField, changePosition: SourceRange => SourceRange = null)
  extends CustomGrammar[WithMap[Unit]]
{
  override def children: Seq[BiGrammar[_]] = Seq(inner)

  override def withChildren(newChildren: Seq[BiGrammar[_]]) = As(newChildren.head.asInstanceOf[BiGrammar[Value]], field, changePosition)

  override def containsParser(recursive: BiGrammar[_] => Boolean): Boolean = recursive(inner)

  override def print(toDocumentInner: BiGrammar[_] => ResponsiveDocument): ResponsiveDocument = withParenthesis(inner) ~ s".As($field)"

  override def createPrinter(recursive: ToPrinterCached): Printer[WithMap[Unit]] = new AsPrinter(recursive(inner), field)

  override def toParser(recursive: Rec): Self[WithMap[Unit]] = {
    recursive(inner).withRange[WithMap[Unit]]((left, right, result: Value) => {
      var range = SourceRange(left.position, right.position)
      if (changePosition != null)
        range = changePosition(range)
      WithMap(Unit, Map(field -> result, FieldPosition(field) -> range))
    })
  }
}
