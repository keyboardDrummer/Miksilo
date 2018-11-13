package core.parsers

trait ParserWriter {
  type Input <: ParseInput

  type Parser[Result] = core.parsers.Parser[Input, Result]
  type ParseState = core.parsers.ParseState[Input]
  type ParseResult[+Result] = core.parsers.ParseResult[Input, Result]
  type ParseSuccess[+Result] = core.parsers.ParseSuccess[Input, Result]
  type ParseFailure[+Result] = core.parsers.ParseFailure[Input, Result]

  def succeed[Result](value: Result): Return[Input, Result] = Return[Input, Result](value)
  def fail(message: String): Fail[Input] = Fail[Input](message)
}