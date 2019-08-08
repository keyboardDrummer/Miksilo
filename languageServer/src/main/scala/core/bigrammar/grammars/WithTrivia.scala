package core.bigrammar.grammars

import core.bigrammar.BiGrammarToParser.{Result, mergeNamedValues}
import core.bigrammar.{BiGrammar, BiGrammarToParser, WithMap}
import core.bigrammar.printer.Printer.NodePrinter
import core.responsiveDocument.ResponsiveDocument
import util.Utility

//class WithTrivia(inner: BiGrammar, trivia: BiGrammar = ParseWhiteSpace, horizontal: Boolean = true)
//  extends BiSequence(trivia, inner, BiSequence.ignoreLeft, horizontal) {
//  def getGrammar: BiGrammar = second
//
//  override def containsParser(recursive: BiGrammar => Boolean): Boolean = true
//
//  override protected def getLeftChildren(recursive: BiGrammar => Seq[BiGrammar]) = recursive(inner) //TODO maybe if we can remove all the WithTrivia's first in TriviaInsideNode we wouldn't need this hack.
//}

case class WithTrivia(var inner: BiGrammar, var trivia: BiGrammar = ParseWhiteSpace, horizontal: Boolean = true) extends CustomGrammar {
  override def print(toDocumentInner: BiGrammar => ResponsiveDocument) = toDocumentInner(inner)

  override def createPrinter(recursive: BiGrammar => NodePrinter) = {
    recursive(new BiSequence(trivia, inner, BiSequence.ignoreLeft, horizontal))
  }

  override def toParser(recursive: BiGrammar => BiGrammarToParser.ParserBuilder[Result]) =
    new WithTriviaParser(recursive(inner), recursive(trivia))

  override def children = Seq(inner, trivia)

  override def withChildren(newChildren: Seq[BiGrammar]) = WithTrivia(newChildren.head, newChildren(1), horizontal)

  override def containsParser(recursive: BiGrammar => Boolean) = true

  override protected def getLeftChildren(recursive: BiGrammar => Seq[BiGrammar]) = recursive(inner)
}

class MustProgressParser[Result](original: BiGrammarToParser.Self[Result]) extends BiGrammarToParser.ParserBuilderBase[Result] {
  override def getParser(recursive: BiGrammarToParser.GetParser) = {
    val originalParser = recursive(original)
    new BiGrammarToParser.Parser[Result] {
      override def apply(input: BiGrammarToParser.Reader, state: BiGrammarToParser.ParseState) = {
        originalParser.apply(input, state).flatMapReady(ready => {
          if (ready.remainder == input)
            BiGrammarToParser.SREmpty
          else
            BiGrammarToParser.singleResult(ready)
        }, uniform = true)
      }
    }
  }

  override def getMustConsume(cache: BiGrammarToParser.ConsumeCache) = true

  override def leftChildren = List(original)

  override def children = List(original)
}

class WithTriviaParser(original: BiGrammarToParser.Self[Result], triviasParserBuilder: BiGrammarToParser.ParserBuilder[Result])
  extends BiGrammarToParser.ParserBuilderBase[Result] {

  override def getParser(recursive: BiGrammarToParser.GetParser) = {
    val bothParserBuilder = BiGrammarToParser.leftRightSimple(new MustProgressParser(triviasParserBuilder), new MustProgressParser(original),
      (firstResult: Result, secondResult: Result) => {
      val resultValue = secondResult.value
      val resultMap = Utility.mergeMaps(firstResult.namedValues, secondResult.namedValues, mergeNamedValues)
      WithMap[Any](resultValue, resultMap)
    })
    recursive(bothParserBuilder | original)
  }

  override def getMustConsume(cache: BiGrammarToParser.ConsumeCache) = cache(original)

  override def leftChildren = List(original)

  override def children = List(original)
}