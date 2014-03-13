package grammar

trait ParseResult
case class ParseSuccess(results: Seq[Any], remainingStream: Stream[Any]) extends ParseResult
object ParseFailure extends ParseResult

trait Grammar {
  val parent = this
  def parse(input: Stream[Any]) : ParseResult
  def printer : Grammar
}

case class Many(inner: Grammar) extends Grammar {
  lazy val grammar = Choice(Many(inner),Produce(Seq()))

  override def parse(input: Stream[Any]): ParseResult = grammar.parse(input)

  override def printer: Grammar = grammar.printer
}

case class Some(inner: Grammar) extends Grammar {
  lazy val grammar = Sequence(inner, Many(inner))

  override def parse(input: Stream[Any]): ParseResult = grammar.parse(input)

  override def printer: Grammar = grammar.printer
}

case class Sequence(first: Grammar, second: Grammar) extends Grammar {
  override def parse(input: Stream[Any]): ParseResult =
    first.parse(input) match {
      case ParseSuccess(firstResult,firstRest) => second.parse(firstRest) match {
          case ParseSuccess(secondResult,secondRest) => ParseSuccess(firstResult ++ secondResult, secondRest)
          case ParseFailure => ParseFailure
      }
      case ParseFailure => ParseFailure
    }

  override def printer: Grammar = Sequence(first.printer,second.printer)
}

case class Produce(result: Seq[Any]) extends Grammar {
  override def parse(input: Stream[Any]): ParseResult = ParseSuccess(result,input)

  override def printer: Grammar = Consume(result)
}

case class Consume(value: Seq[Any]) extends Grammar{
  override def parse(input: Stream[Any]): ParseResult = {
    val (prefix,rest) = input.splitAt(value.length)
    if (prefix != value)
      return ParseFailure
    ParseSuccess(Seq(), rest)
  }

  override def printer: Grammar = Produce(value)
}

case class Choice(left: Grammar, right: Grammar) extends Grammar{
  override def parse(input: Stream[Any]): ParseResult = {
    left.parse(input) match {
      case success: ParseSuccess => success
      case ParseFailure => right.parse(input)
    }
  }

  override def printer: Grammar = Choice(left.printer,right.printer)
}
