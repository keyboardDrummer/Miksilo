package core.parsers

class PositionParser[Input <: ParseInput] extends Parser[Input, Input] {

  override def parseNaively(input: Input, state: ParseState): ParseResult[Input] = {
    ParseSuccess[Input, Input](input, input, NoFailure)
  }

  override def getDefault(cache: DefaultCache): Option[Input] = None
}
