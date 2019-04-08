package core.parsers.core

trait MonadicParser extends ParserWriter {

  implicit class MonadicParserExtensions[+Result](parser: Self[Result]) extends ParserExtensions(parser) {

    def flatMap[NewResult](getRight: Result => Self[NewResult]): Self[NewResult] =
      MonadicParser.this.flatMap(parser, getRight)
  }

  def flatMap[Result, NewResult](left: Self[Result], getRight: Result => Self[NewResult]): Self[NewResult]
}
