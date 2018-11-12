package core.parsers

class BiggestOfTwo[Input <: ParseInput, +First <: Result, +Second <: Result, +Result](first: Parser[Input, First], _second: => Parser[Input, Second])
  extends Parser[Input, Result] {
  lazy val second = _second

  override def parseNaively(input: Input, state: ParseState): ParseResult[Result] = {
    val firstResult = first.parseCached(input, state)
    val secondResult = second.parseCached(input, state)
    (firstResult, secondResult) match {
      case (firstSuccess: ParseSuccess[Result], secondSuccess: ParseSuccess[Result]) =>
        if (firstSuccess.remainder.offset > secondSuccess.remainder.offset)
          firstSuccess.addFailure(secondSuccess.biggestFailure)
        else
          secondSuccess.addFailure(firstSuccess.biggestFailure)
      case (firstFailure: ParseFailure[Result], secondSuccess: ParseSuccess[Result]) =>
        secondSuccess.addFailure(firstFailure)
      case (firstSuccess: ParseSuccess[Result], secondFailure: ParseFailure[Result]) =>
        firstSuccess.addFailure(secondFailure)
      case (firstFailure: ParseFailure[Result], secondFailure: ParseFailure[Result]) =>
        firstFailure.getBiggest(secondFailure)
      case _ => throw new Exception("can not occur")
    }
  }

  override def getDefault(cache: DefaultCache): Option[Result] = {
    val value: Option[First] = cache(first)
    value.orElse(cache(second))
  }
}
