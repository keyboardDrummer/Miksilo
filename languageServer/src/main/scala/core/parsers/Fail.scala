package core.parsers

case class Fail[Input <: ParseInput](message: String) extends Parser[Input, Nothing] {
  override def parseNaively(input: Input, cache: ParseState): ParseResult[Nothing] = ParseFailure(None, input, message)

  override def getDefault(cache: DefaultCache): Option[Nothing] = None
}
