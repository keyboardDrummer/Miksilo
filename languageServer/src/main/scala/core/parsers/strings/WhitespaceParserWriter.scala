package core.parsers.strings

import core.parsers.core.Processor

trait WhitespaceParserWriter extends StringParserWriter {

  final val whiteSpace: Self[String] = RegexParser("""\s+""".r, "whitespace", score = -0.001, penaltyOption = None)
  def trivia = whiteSpace

  def oldMany[Result, Sum](original: ParserBuilder[Result],
                        zero: Sum, reduce: (Result, Sum) => Sum,
                        parseGreedy: Boolean = true) = {
    lazy val result: Self[Sum] = choice(new Sequence(original, result, combineFold(zero, reduce)), succeed(zero), firstIsLonger = parseGreedy)
    result
  }

  // Not parsing greedily allow us to not parse whiteSpace, which allow us to place missing input errors before whitespace.
  lazy val trivias: Self[List[String]] = oldMany(trivia, List.empty, (h: String, t: List[String]) =>  h :: t, parseGreedy = false)

  override def leftRight[Left, Right, Result](left: ParserBuilder[Left], right: => ParserBuilder[Right],
                                              combine: (Option[Left], Option[Right]) => Option[Result]) =  {
    new Sequence(left, new Sequence(trivias, right, Processor.ignoreLeft[Option[Any], Option[Right]]), combine)
  }
}
