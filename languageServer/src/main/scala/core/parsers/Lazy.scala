package core.parsers

class Lazy[Input <: ParseInput, +Result](_inner: => Parser[Input, Result]) extends Parser[Input, Result] {
  lazy val inner: Parser[Result] = _inner

  // We skip caching and left-recursion handling on lazy by redirecting parseCaching to the inner.
  override def parseCached(input: Input, cache: ParseState): ParseResult[Result] = inner.parseCached(input, cache)
  override def parseIteratively(input: Input, cache: ParseState): ParseResult[Result] = inner.parseIteratively(input, cache)
  override def parseNaively(input: Input, cache: ParseState): ParseResult[Result] = inner.parseNaively(input, cache)

  override def getDefault(cache: DefaultCache): Option[Result] = cache(inner)
}
