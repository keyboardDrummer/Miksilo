package core.bigrammar.grammars

import core.bigrammar.BiGrammarToParser.{AnyWithMap, Result}
import core.bigrammar.PrintBiGrammar.withParenthesis
import core.bigrammar.printer.AsPrinter
import core.bigrammar.printer.Printer.NodePrinter
import core.bigrammar.{BiGrammar, BiGrammarToParser, WithMap}
import core.language.node.{NodeField, SourceRange}
import core.responsiveDocument.ResponsiveDocument
import langserver.types.Position
import languageServer.HumanPosition

case class As(var inner: BiGrammar, field: NodeField) extends CustomGrammar
{
  override def children: Seq[BiGrammar] = Seq(inner)

  override def withChildren(newChildren: Seq[BiGrammar]) = As(newChildren.head, field)

  override def containsParser(recursive: BiGrammar => Boolean): Boolean = recursive(inner)

  override def print(toDocumentInner: BiGrammar => ResponsiveDocument): ResponsiveDocument = withParenthesis(inner) ~ s".As($field)"

  override def createPrinter(recursive: BiGrammar => NodePrinter): NodePrinter = new AsPrinter(recursive(inner), field)

  override def toParser(recursive: BiGrammar => BiGrammarToParser.EditorParser[Result]): BiGrammarToParser.EditorParser[Result] = {
    val innerParser: BiGrammarToParser.EditorParser[Result] = recursive(inner).
      map(result => result.map { case WithMap(value, state) =>
      WithMap(Unit, state + (field -> value))
    })
    innerParser.withRange[Result]((left, right, result: Result) => result.map[AnyWithMap]({ case WithMap(value, state) =>
      val start: Position = new HumanPosition(left.position.line, left.position.column)
      val end: Position = new HumanPosition(right.position.line, right.position.column)
      WithMap[Any](value, state + (FieldPosition(field) -> SourceRange(start, end)))
    }))
  }
}
