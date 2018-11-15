package core.bigrammar.grammars

import core.bigrammar.BiGrammarToParser.Result
import core.bigrammar.PrintBiGrammar.withParenthesis
import core.bigrammar.printer.AsPrinter
import core.bigrammar.printer.Printer.NodePrinter
import core.bigrammar.{BiGrammar, WithMap}
import core.language.node.{NodeField, SourceRange}
import core.parsers.{DefaultCache, NoFailure, ParseSuccess}
import core.parsers.strings.StringReader
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

  override def toParser(recursive: BiGrammar => Parser[Result]): Parser[Result] = {
    val innerParser = recursive(inner)
    new core.parsers.Sequence[StringReader, Option[Position], Option[Position] => Result, Result](position.option, new Parser[Option[Position] => Result] {
      override def parseNaively(input: StringReader, parseState: ParseState): ParseResult[Option[Position] => Result] = {
        val result = innerParser.parseCached(input, parseState)
        result match {
          case success: ParseSuccess[Result] => ParseSuccess(addPosition(success.result, success), success.remainder, success.biggestFailure match {
            case NoFailure => NoFailure
            case failure: ParseFailure[Result] => failure.map(r => addPosition(r, failure))
          })
          case failure: ParseFailure[Result] => failure.map(r => addPosition(r, failure))
        }
      }

      def addPosition(result: Result, parseResult: ParseResult[Result]): Option[Position] => Result =  {
        maybeStart: Option[Position] => result.map { case WithMap(value, state) =>
          val end: Position = new HumanPosition(parseResult.remainder.position.line, parseResult.remainder.position.column)
          var map: Map[Any, Any] = state + (field -> value)
          maybeStart.foreach(start => map += (FieldPosition(field) -> SourceRange(start, end)))
          WithMap(inner, map)
        }
      }

      override def getDefault(cache: DefaultCache): Option[Option[Position] => Result] =
        innerParser.getDefault(cache).map(r => _ => r.map { case WithMap(value, state) =>
          val map = state +  (field -> value)
          WithMap(inner, map)
        })
    }, (start, rf) => rf(start))


//    val result = for {
//      start <- position.option
//      inner <- innerParser
//      end <- position.option
//    } yield {
//      inner.map { case WithMap(value, state) =>
//        var map = state + (field -> value)
//        if (start.nonEmpty && end.nonEmpty)
//          map += (FieldPosition(field) -> SourceRange(start.get, end.get))
//        WithMap(inner, map)
//      }
//    }
//    result
//    new Parser[Result] {
//      override def parseNaively(input: StringReader, state: ParseState): this.ParseResult[Result] = result.parseNaively(input, state)
//
//      override def getDefault(cache: DefaultCache): Option[Result] = innerParser.getDefault(cache).map(r => r.map { case WithMap(value, state) =>
//        val map = state +  (field -> value)
//        WithMap(inner, map)
//      })
//    }
  }
}
