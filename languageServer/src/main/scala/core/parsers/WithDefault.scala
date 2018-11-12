package core.parsers

case class WithDefault[Input <: ParseInput, Result](original: Parser[Input, Result], _default: Result) extends Parser[Input, Result] {
  override def parseNaively(input: Input, cache: ParseState): ParseResult[Result] = {
    original.parseCached(input, cache) match {
      case failure: ParseFailure[Result] if failure.partialResult.isEmpty =>
        new ParseFailure[Result](Some(_default), failure.remainder, failure.message)
      case x => x
    }
  }

  override def getDefault(cache: DefaultCache): Option[Result] = Some(_default)
}
