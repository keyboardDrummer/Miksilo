package core.bigrammar.grammars

import core.bigrammar.BiGrammarToParser._
import core.bigrammar.PrintBiGrammar.withParenthesis
import core.bigrammar.printer.AsPrinter
import core.bigrammar.printer.Printer.NodePrinter
import core.bigrammar.{BiGrammar, WithMap}
import core.language.node.NodeField
import core.responsiveDocument.ResponsiveDocument
import languageServer.SourceRange

case class As(var inner: BiGrammar, field: NodeField, changePosition: SourceRange => SourceRange = null) extends CustomGrammar
{
  override def children: Seq[BiGrammar] = Seq(inner)

  override def withChildren(newChildren: Seq[BiGrammar]) = As(newChildren.head, field, changePosition)

  override def containsParser(recursive: BiGrammar => Boolean): Boolean = recursive(inner)

  override def print(toDocumentInner: BiGrammar => ResponsiveDocument): ResponsiveDocument = withParenthesis(inner) ~ s".As($field)"

  override def createPrinter(recursive: BiGrammar => NodePrinter): NodePrinter = new AsPrinter(recursive(inner), field)

  override def toParser(recursive: BiGrammar => Self[Result]): Self[Result] = {
    recursive(inner).withRange[Result]((left, right, result: Result) => {
      var range = SourceRange(left.position, right.position)
      if (changePosition != null)
        range = changePosition(range)
      WithMap[Any](Unit, result.namedValues + (field -> result.value) + (FieldPosition(field) -> range))
    })
  }
}
