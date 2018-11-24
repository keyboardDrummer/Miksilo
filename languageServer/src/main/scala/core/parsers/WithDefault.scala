package core.parsers

case class WithDefault[Input <: ParseInput, +Result](original: Parser[Input, Result], _getDefault: DefaultCache => Option[Result])
  extends Parser[Input, Result] {
  override def parseNaively(input: Input, state: ParseState): ParseResult[Result] = {
    original.parseCached(input, state) match {
      case failure: ParseFailure[Result] if failure.partialResult.isEmpty =>
        new ParseFailure[Result](_getDefault(state.defaultCache), failure.remainder, failure.message)
      case x => x
    }
  }

  override def getDefault(cache: DefaultCache): Option[Result] =
    _getDefault(cache)
}
