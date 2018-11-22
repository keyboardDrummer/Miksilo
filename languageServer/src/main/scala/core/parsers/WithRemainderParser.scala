package core.parsers

class WithRemainderParser[Input <: ParseInput, Result](original: Parser[Input, Result])
  extends Parser[Input, (Result, Input)] {

  override def parseNaively(input: Input, parseState: ParseState): ParseResult[(Result, Input)] = {
    val parseResult = original.parseCached(input, parseState)

    parseResult.map(result => (result, parseResult.remainder))
  }

  override def getDefault(cache: DefaultCache): Option[(Result, Input)] = None
}
