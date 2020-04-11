package miksilo.editorParser.parsers.strings

import miksilo.editorParser.parsers.core.Processor

trait WhitespaceParserWriter extends StringParserWriter {

  final val whiteSpace: Parser[String] = RegexParser("""\s+""".r, "whitespace", score = 0, penaltyOption = None)
  def trivia = whiteSpace

  def oldSome[Result, Sum](original: ParserBuilder[Result],
                           zero: Sum, reduce: (Result, Sum) => Sum,
                           parseGreedy: Boolean = true) = {
    leftRight(original, oldMany(original, zero, reduce, parseGreedy), combineFold(zero, reduce))
  }

  def oldMany[Result, Sum](original: ParserBuilder[Result],
                           zero: Sum, reduce: (Result, Sum) => Sum,
                           parseGreedy: Boolean = true) = {
    lazy val result: Parser[Sum] = choice(new Sequence(original, result, combineFold(zero, reduce)), succeed(zero), firstIsLonger = parseGreedy)
    result
  }

  override def many[Element, Sum](element: ParserBuilder[Element],
                                  zero: Sum, append: (Element, Sum) => Sum,
                                  parseGreedy: Boolean = true) = {
    super.many(new LeftIfRightMoved(someTrivias, element, Processor.ignoreLeft[Option[Any], Option[Element]]), zero, append, parseGreedy)
  }

  private lazy val someTrivias: Parser[Vector[String]] = oldSome(trivia, Vector.empty, (h: String, t: Vector[String]) => t.appended(h))
  lazy val trivias: Parser[Vector[String]] = oldMany(trivia, Vector.empty, (h: String, t: Vector[String]) => t.appended(h))

  override def leftRight[Left, Right, Result](left: ParserBuilder[Left], right: => ParserBuilder[Right],
                                              combine: (Option[Left], Option[Right]) => Option[Result]) =  {
    new Sequence(left, new LeftIfRightMoved(someTrivias, right, Processor.ignoreLeft[Option[Any], Option[Right]]), combine)
  }
}
