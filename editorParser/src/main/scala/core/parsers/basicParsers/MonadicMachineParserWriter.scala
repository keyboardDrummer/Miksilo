package core.parsers.basicParsers

import core.parsers.core.MonadicParser

trait MonadicMachineParserWriter extends MachineParserWriter with MonadicParser {

  override def flatMap[Result, NewResult](left: Parser[Result], getRight: Result => Parser[NewResult]): Parser[NewResult] =
    new FlatMap(left, getRight)

  class FlatMap[Result, NewResult](left: Parser[Result], getRight: Result => Parser[NewResult]) extends Parser[NewResult] {

    override def parse(input: Input): ParseResult[NewResult] = {
      left.parse(input).successOption match {
        case None => failureSingleton
        case Some(leftSuccess) => getRight(leftSuccess.result).parse(leftSuccess.remainder)
      }
    }
  }
}
