package core.parsers

case class Return[Input <: ParseInput, Result](value: Result) extends Parser[Input, Result] {
  override def parseNaively(inputs: Input, cache: ParseState): ParseResult[Result] = ParseSuccess(value, inputs, NoFailure)

  override def getDefault(cache: DefaultCache): Option[Result] = Some(value)
}
