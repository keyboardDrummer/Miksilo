package core.parsers.strings

import core.parsers.core.Processor

trait WhitespaceParserWriter extends StringParserWriter {

  final val whiteSpace: Parser[String] = RegexParser("""\s+""".r, "whitespace", score = 0, penaltyOption = None)
  def trivia = whiteSpace

  def oldSome[Result, Sum](original: ParserBuilder[Result],
                           zero: Sum, reduce: (Result, Sum) => Sum,
                           parseGreedy: Boolean = true) = {
    lazy val result: Parser[Sum] = choice(new Sequence(original, result, combineFold(zero, reduce)), succeed(zero), firstIsLonger = parseGreedy)
    leftRight(original, result, combineFold(zero, reduce))
  }


  lazy val trivias: Parser[List[String]] = oldSome(trivia, List.empty, (h: String, t: List[String]) =>  h :: t)

  override def leftRight[Left, Right, Result](left: ParserBuilder[Left], right: => ParserBuilder[Right],
                                              combine: (Option[Left], Option[Right]) => Option[Result]) =  {
    new Sequence(left, new LeftIfRightMoved(trivias, right, Processor.ignoreLeft[Option[Any], Option[Right]]), combine)
  }
}
