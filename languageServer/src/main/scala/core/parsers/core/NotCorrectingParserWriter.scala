package core.parsers.core

trait NotCorrectingParserWriter extends ParserWriter {
  type Self[+Result] = Parser[Result]

  def succeed[Result](result: Result): Self[Result] = new SuccessParser(result)

  class SuccessParser[Result](result: Result) extends ParserBase[Result] {
    override def parseInternal(input: Input) = newSuccess(result, input)

    override def children = List.empty
  }

}
