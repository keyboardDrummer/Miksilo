package core.parsers

class Lazy[Input <: ParseInput, +Result](_inner: => Parser[Input, Result]) extends Parser[Input, Result] {
  lazy val inner: Parser[Result] = _inner

  override def parseCached(inputs: Input, cache: ParseState): ParseResult[Result] = inner.parseCached(inputs, cache)
  override def parse(inputs: Input, cache: ParseState): ParseResult[Result] = throw new Exception("should not be called")

  override def getDefault(cache: DefaultCache): Option[Result] = cache(inner)
}
