package core.parsers

class OrElse[Input <: ParseInput, +First <: Result, +Second <: Result, +Result](first: Parser[Input, First], _second: => Parser[Input, Second])
  extends Parser[Input, Result] {
  lazy val second = _second

  override def parse(input: Input, cache: ParseState): ParseResult[Result] = {
    val firstResult = first.parseCached(input, cache)
    firstResult match {
      case _: ParseSuccess[Result] => firstResult
      case firstFailure: ParseFailure[Result] =>
        val secondResult = second.parseCached(input, cache)
        secondResult match {
          case secondSuccess: ParseSuccess[Result] =>
            val biggestFailure = firstFailure.getBiggest(secondSuccess.biggestFailure)
            ParseSuccess(secondSuccess.result, secondSuccess.remainder, biggestFailure)
          case secondFailure: ParseFailure[Result] =>
            firstFailure.getBiggest(secondFailure)
        }
    }
  }

  override def getDefault(cache: DefaultCache): Option[Result] = {
    val value: Option[First] = cache(first)
    value.orElse(cache(second))
  }
}
