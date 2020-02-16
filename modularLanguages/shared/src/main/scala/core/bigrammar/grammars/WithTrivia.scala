package core.bigrammar.grammars

import core.bigrammar.BiGrammarToParser.Result
import core.bigrammar.{BiGrammar, BiGrammarToParser, WithMap}
import core.bigrammar.printer.Printer.NodePrinter
import core.parsers.core.ParseText
import core.parsers.editorParsers.{ParseResults, ReadyParseResult, SREmpty}
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

class WithTriviaParser(original: BiGrammarToParser.Parser[Result], triviasParserBuilder: BiGrammarToParser.ParserBuilder[Result])
  extends BiGrammarToParser.ParserBuilderBase[Result] {

  import BiGrammarToParser._

  override def getParser(textContainer: ParseText, recursive: BiGrammarToParser.GetParser): BuiltParser[Result] = {
    val parseTrivias = recursive(triviasParserBuilder)
    val parseOriginal = recursive(original)

    new BuiltParser[Result] {
      override def apply(input: Input, state: ParseState): ParseResults[Input, Result] = {
        val leftResults = parseTrivias(input, state)

        def rightFromLeftReady(leftReady: ReadyParseResult[Input, Result]): ParseResults[Input, Result] = {
          if (leftReady.history.flawed)
            return SREmpty.empty // Do not error correct Trivia.

          val rightResult = parseOriginal(leftReady.remainder, state)
          rightResult.flatMapReady(rightReady => {
            if (input != leftReady.remainder && leftReady.remainder == rightReady.remainder) {
              SREmpty.empty // To avoid ambiguities, trivia may only occur before parsed input, not before inserted input.
            } else {
              val value = leftReady.resultOption.flatMap(leftValue =>
                rightReady.resultOption.map(rightValue => {
                  val resultMap = Utility.mergeMaps(leftValue.namedValues, rightValue.namedValues, mergeNamedValues)
                  WithMap[Any](rightValue.value, resultMap)
                })
              )

              singleResult(ReadyParseResult(value,
                rightReady.remainder,
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