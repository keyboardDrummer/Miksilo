package core.parsers

case class Filter[Input <: ParseInput, Other, +Result <: Other](original: Parser[Input, Result], predicate: Other => Boolean, getMessage: Other => String) extends Parser[Input, Result] {
  override def parse(input: Input, state: ParseState): ParseResult[Result] = original.parse(input, state) match {
    case success: ParseSuccess[Result] =>
      if (predicate(success.result)) success
      else ParseFailure(this.getDefault(state), success.remainder, getMessage(success.result)).getBiggest(success.biggestFailure)
    case failure: ParseFailure[Result] =>
      ParseFailure(failure.partialResult.filter(predicate), failure.remainder, failure.message)
  }

  override def getDefault(cache: DefaultCache): Option[Result] =
    original.getDefault(cache).filter(predicate)
}
