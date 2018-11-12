package core.parsers

case class Many[Input <: ParseInput, +Result](single: Parser[Input, Result]) extends Parser[Input, List[Result]] { //TODO kan ik Many ook in termen van de anderen opschrijven?
  override def parse(inputs: Input, cache: ParseState): ParseResult[List[Result]] = {
    val result = single.parseCached(inputs, cache)
    result match {
      case success: ParseSuccess[Result] =>
        parseCached(success.remainder, cache).map(r => success.result :: r)
      case failure: ParseFailure[Result] =>
        val partialResult = failure.partialResult.fold(List.empty[Result])(r => List(r))
        val newFailure = ParseFailure[Input, List[Result]](Some(partialResult), failure.remainder, failure.message)
        ParseSuccess(List.empty[Result], inputs, newFailure) // Voor het doorparsen kan ik kijken of de failure iets geparsed heeft, en zo ja verder parsen op de remainder.
    }
  }

  override def getDefault(cache: DefaultCache): Option[List[Result]] = Some(List.empty[Result])
}

case class SomeParser[Input <: ParseInput, Result](single: Parser[Input, Result]) extends
  Sequence[Input, Result, List[Result], List[Result]](single, Many(single), (f, rest) => f :: rest)