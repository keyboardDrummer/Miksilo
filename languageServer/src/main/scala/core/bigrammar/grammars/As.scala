package core.bigrammar.grammars

import core.bigrammar.BiGrammarToParser._
import core.bigrammar.PrintBiGrammar.withParenthesis
import core.bigrammar.printer.AsPrinter
import core.bigrammar.printer.Printer.NodePrinter
import core.bigrammar.{BiGrammar, WithMap}
import core.language.node.{NodeField, SourceRange}
import core.responsiveDocument.ResponsiveDocument

case class As(var inner: BiGrammar, field: NodeField, changePosition: SourceRange => SourceRange = null) extends CustomGrammar
{
  override def children: Seq[BiGrammar] = Seq(inner)

  override def withChildren(newChildren: Seq[BiGrammar]) = As(newChildren.head, field, changePosition)

  override def containsParser(recursive: BiGrammar => Boolean): Boolean = recursive(inner)

  override def print(toDocumentInner: BiGrammar => ResponsiveDocument): ResponsiveDocument = withParenthesis(inner) ~ s".As($field)"

  override def createPrinter(recursive: BiGrammar => NodePrinter): NodePrinter = new AsPrinter(recursive(inner), field)

  override def toParser(recursive: BiGrammar => Self[Result]): Self[Result] = {
    recursive(inner).map(x => x)
//    val innerParser: Self[Result] = recursive(inner) //.map(x => x)
//      map(result => result.map { case WithMap(value, state) =>
//      WithMap(Unit, state + (field -> value))
//    })
//    innerParser.withRange[Result]((left, right, result: Result) => result.map[AnyWithMap]({ case WithMap(value, state) =>
//      var range = SourceRange(left.position, right.position)
//      if (changePosition != null)
//        range = changePosition(range)
//      WithMap[Any](value, state + (FieldPosition(field) -> range))
//    }))
  }
}
