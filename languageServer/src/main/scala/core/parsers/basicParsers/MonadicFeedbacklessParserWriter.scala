package core.parsers.basicParsers

import core.parsers.core.MonadicParser

trait MonadicFeedbacklessParserWriter extends FeedbacklessParserWriter with MonadicParser {

  override def flatMap[Result, NewResult](left: Parser[Result], getRight: Result => Parser[NewResult]): Parser[NewResult] =
    new FlatMap(left, getRight)

  class FlatMap[Result, NewResult](left: Parser[Result], getRight: Result => Parser[NewResult]) extends ParserBase[NewResult] {
    override def apply(input: Input) = {
      left.parse(input).successOption match {
        case None => failureSingleton
        case Some(leftSuccess) => getRight(leftSuccess.result).parse(leftSuccess.remainder)
      }
    }

    override def leftChildren = ???

    override def getMustConsume(cache: ConsumeCache) = cache(left)

    override def children = ???
  }
}
