package core.bigrammar.grammars

import core.bigrammar.BiGrammarToParser.Rec
import core.bigrammar.printer.BiGrammarToPrinter.ToPrinterCached
import core.bigrammar.{BiGrammar, BiGrammarToParser, WithMap}
import core.responsiveDocument.ResponsiveDocument
import util.Utility

case class WithTrivia[Value](var inner: BiGrammar[WithMap[Value]], var trivia: BiGrammar[WithMap[Unit]] = ParseWhiteSpace, horizontal: Boolean = true)
  extends CustomGrammar[WithMap[Value]] {
  override def print(toDocumentInner: BiGrammar[_] => ResponsiveDocument) = toDocumentInner(sequence)

  def sequence = new BiSequence[WithMap[Unit], WithMap[Value], WithMap[Value]](trivia, inner, BiGrammarToParser.ignoreLeft, horizontal)
  override def createPrinter(recursive: ToPrinterCached) = {
    recursive(sequence)
  }

  override def toParser(recursive: Rec) =
    new WithTriviaParser(recursive(inner), recursive(trivia))

  override def children = Seq(inner, trivia)

  override def withChildren(newChildren: Seq[BiGrammar[_]]) =
    WithTrivia(newChildren.head.asInstanceOf[BiGrammar[WithMap[Value]]], newChildren(1).asInstanceOf[BiGrammar[WithMap[Unit]]], horizontal)

  override def containsParser(recursive: BiGrammar[_] => Boolean) = true

  override protected def getLeftChildren(recursive: BiGrammar[_] => Seq[BiGrammar[_]]) = recursive(inner)
}

class WithTriviaParser[Value](original: BiGrammarToParser.Self[WithMap[Value]],
                              triviasParserBuilder: BiGrammarToParser.ParserBuilder[WithMap[Unit]])
  extends BiGrammarToParser.ParserBuilderBase[WithMap[Value]] {

  import BiGrammarToParser._

  override def getParser(recursive: GetParser): Parser[WithMap[Value]] = {
    val parseTrivias = recursive(triviasParserBuilder)
    val parseOriginal = recursive(original)

    new Parser[WithMap[Value]] {
      override def apply(input: Input, state: ParseState): SortedParseResults[WithMap[Value]] = {
        val leftResults = parseTrivias.apply(input, state)

        def rightFromLeftReady(leftReady: ReadyParseResult[WithMap[Unit]]): SortedParseResults[WithMap[Value]] = {
          if (leftReady.history.flawed)
            return SREmpty // Do not error correct Trivia.

          val rightResult = parseOriginal(leftReady.remainder, state)
          rightResult.flatMapReady(rightReady => {
            if (input != leftReady.remainder && leftReady.remainder == rightReady.remainder) {
              SREmpty // To avoid ambiguities, trivia may only occur before parsed input, not before inserted input.
            } else {
              val value = leftReady.resultOption.flatMap(leftValue =>
                rightReady.resultOption.map(rightValue => {
                  val resultMap = Utility.mergeMaps(leftValue.namedValues, rightValue.namedValues, mergeNamedValues)
                  WithMap[Value](rightValue.value, resultMap)
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