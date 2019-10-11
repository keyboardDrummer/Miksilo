package core.parsers.strings

import core.parsers.core.Processor

trait WhitespaceParserWriter extends StringParserWriter {

  val whiteSpace = RegexParser("""\s*""".r, "whitespace", score = 0)

  override def leftRight[Left, Right, Result](left: ParserBuilder[Left], right: => ParserBuilder[Right],
                                              combine: (Option[Left], Option[Right]) => Option[Result]) =  {
    new Sequence(left, new Sequence(whiteSpace, right, Processor.ignoreLeft[Option[String], Option[Right]]), combine)
  }
}
