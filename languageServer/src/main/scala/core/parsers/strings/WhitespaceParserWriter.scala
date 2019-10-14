package core.parsers.strings

import core.parsers.core.Processor

trait WhitespaceParserWriter extends StringParserWriter {

  val whiteSpace: Self[Any] = RegexParser("""\s*""".r, "whitespace", score = 0)
  val trivia: Self[Any] = whiteSpace

  override def leftRight[Left, Right, Result](left: ParserBuilder[Left], right: => ParserBuilder[Right],
                                              combine: (Option[Left], Option[Right]) => Option[Result]) =  {
    new Sequence(left, new Sequence(trivia, right, Processor.ignoreLeft[Option[Any], Option[Right]]), combine)
  }
}
