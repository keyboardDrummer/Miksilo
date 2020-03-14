package core.bigrammar.grammars

import core.bigrammar.BiGrammarToParser.Result
import core.bigrammar.printer.Printer.NodePrinter
import core.bigrammar.{BiGrammar, BiGrammarToParser, WithMap}
import core.parsers.core.{OffsetPointer, TextPointer}
import core.parsers.editorParsers._
import core.responsiveDocument.ResponsiveDocument
import util.Utility

case class WithTrivia(var inner: BiGrammar, var trivia: BiGrammar = ParseWhiteSpace, horizontal: Boolean = true) extends CustomGrammar {
  override def print(toDocumentInner: BiGrammar => ResponsiveDocument) = toDocumentInner(sequence)

  def sequence = new BiSequence(trivia, inner, BiSequence.ignoreLeft, horizontal)
  override def createPrinter(recursive: BiGrammar => NodePrinter) = {
    recursive(sequence)
  }

  override def toParser(recursive: BiGrammar => BiGrammarToParser.ParserBuilder[Result]) =
    new WithTriviaParser(recursive(inner), recursive(trivia))

  override def children = Seq(inner, trivia)

  override def withChildren(newChildren: Seq[BiGrammar]) = WithTrivia(newChildren.head, newChildren(1), horizontal)

  override def containsParser(recursive: BiGrammar => Boolean) = true

  override protected def getLeftChildren(recursive: BiGrammar => Seq[BiGrammar]) = recursive(inner)
}

case class UpdateLatestRemainder[State, Result](remainder: OffsetPointer) extends ParseResults[State, Result] {

  override def merge[Other >: Result](other: ParseResults[State, Other], mergeDepth: Int,
                                       bests: Map[Int, Double] = Map.empty): ParseResults[State, Other] = {
    other match {
      case _: SREmpty[State] => this
      case cons: SRCons[State, Other] =>
        new SRCons(cons.head, cons.tailDepth + 1, this.merge(cons.tail, mergeDepth + 1, bests))
      case latestRemainder2: UpdateLatestRemainder[State, Result] =>
        if (latestRemainder2.remainder.offset > remainder.offset) latestRemainder2 else this
    }
  }

  override def latestRemainder: OffsetPointer = remainder

  override def mapResult[NewResult](f: LazyParseResult[State, Result] => LazyParseResult[State, NewResult], uniform: Boolean) =
    this.asInstanceOf[ParseResults[State, NewResult]]

  override def flatMap[NewResult](f: LazyParseResult[State, Result] => ParseResults[State, NewResult], uniform: Boolean) =
    this.asInstanceOf[ParseResults[State, NewResult]]

  override def map[NewResult](f: Result => NewResult) =
    this.asInstanceOf[ParseResults[State, NewResult]]

  override def tailDepth = 0

  override def toList = List.empty

  override def nonEmpty = false

  override def pop() = throw new Exception("Can't pop empty results")
}

class WithTriviaParser(original: BiGrammarToParser.Parser[Result], triviasParserBuilder: BiGrammarToParser.ParserBuilder[Result])
  extends BiGrammarToParser.ParserBuilderBase[Result] {

  import BiGrammarToParser._

  override def getParser(recursive: BiGrammarToParser.GetParser): BuiltParser[Result] = {
    val parseTrivias = recursive(triviasParserBuilder)
    val parseOriginal = recursive(original)

    new BuiltParser[Result] {
      override def apply(position: TextPointer, state: State, fixPointState: FixPointState): ParseResults[State, Result] = {
        val leftResults = parseTrivias(position, state, fixPointState)

        def rightFromLeftReady(leftReady: ReadyParseResult[State, Result]): ParseResults[State, Result] = {
          if (leftReady.history.flawed)
            return SREmpty.empty // Do not error correct Trivia.

          val rightResult = parseOriginal(leftReady.remainder, leftReady.state, fixPointState)
          rightResult.flatMapReady(rightReady => {
            val rightWasInserted = leftReady.remainder.offset == rightReady.remainder.offset
            val triviaWasParsed = position.offset != leftReady.remainder.offset
            val failResult = triviaWasParsed && rightWasInserted
            if (failResult) {
              // To avoid ambiguities, trivia may only occur before parsed input, not before inserted input,
              UpdateLatestRemainder(rightReady.remainder)
            } else {
              val value = leftReady.resultOption.flatMap(leftValue =>
                rightReady.resultOption.map(rightValue => {
                  val resultMap = Utility.mergeMaps(leftValue.namedValues, rightValue.namedValues, mergeNamedValues)
                  WithMap[Any](rightValue.value, resultMap)
                })
              )

              singleResult(ReadyParseResult(value,
                rightReady.remainder,
                rightReady.state,
                rightReady.history ++ leftReady.history))
            }
          }, uniform = true)
        }
        leftResults.flatMapReady(rightFromLeftReady, uniform = false)
      }
    }
  }

  override def getMustConsume(cache: BiGrammarToParser.ConsumeCache) = cache(original)

  override def leftChildren = List(triviasParserBuilder, original)

  override def children = List(triviasParserBuilder, original)
}