package core.bigrammar.grammars

import core.bigrammar.BiGrammarToParser.Result
import core.bigrammar.PrintBiGrammar.withParenthesis
import core.bigrammar.{BiGrammar, BiGrammarToParser, WithMapG}
import core.bigrammar.printer.AsPrinter
import core.bigrammar.printer.Printer.NodePrinter
import core.language.node.{NodeField, SourceRange}
import core.responsiveDocument.ResponsiveDocument
import langserver.types.Position

case class As(var inner: BiGrammar, field: NodeField) extends CustomGrammar
{
  override def children: Seq[BiGrammar] = Seq(inner)

  override def withChildren(newChildren: Seq[BiGrammar]) = As(newChildren.head, field)

  override def containsParser(recursive: BiGrammar => Boolean): Boolean = recursive(inner)

  override def print(toDocumentInner: BiGrammar => ResponsiveDocument): ResponsiveDocument = withParenthesis(inner) ~ s".As($field)"

  override def createPrinter(recursive: BiGrammar => NodePrinter): NodePrinter = new AsPrinter(recursive(inner), field)

  override def toParser(recursive: BiGrammar => BiGrammarToParser.Parser[Result]): BiGrammarToParser.Parser[Result] = {
    val innerParser = recursive(inner)
    new BiGrammarToParser.Sequence(BiGrammarToParser.position ~ innerParser, BiGrammarToParser.position, (t: (Position, Result), end: Position) => {
      t._2.map { case WithMapG(v, state) => WithMapG(inner, state +
        (field -> v) +
        (FieldPosition(field) -> SourceRange(t._1, end)))
      }
    })
  }
}
