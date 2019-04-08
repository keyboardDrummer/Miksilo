package core.parsers.core

trait NotCorrectingParserWriter extends ParserWriter {
  type Self[+Result] = Parser[Result]

  def succeed[Result](result: Result): Self[Result] = new SuccessParser(result)

  class SuccessParser[Result](result: Result) extends ParserBase[Result] with LeafParser[Result] {
    override def apply(input: Input) = newSuccess(result, input)

    override def getMustConsume(cache: ConsumeCache) = false
  }

}
