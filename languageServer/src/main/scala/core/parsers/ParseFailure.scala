package core.parsers

case class ParseFailure[Input <: ParseInput, +Result](partialResult: Option[Result], remainder: Input, message: String)
  extends ParseResult[Input, Result] with OptionFailure[Result] {

  override def map[NewResult](f: Result => NewResult): ParseFailure[Input, NewResult] =
    ParseFailure(partialResult.map(r => f(r)), remainder, message)

  override def offset: Int = remainder.offset

  def getBiggest[Other >: Result](other: OptionFailure[Other]): ParseFailure[Input, Other] = {
    if (offset > other.offset) this else other.asInstanceOf[ParseFailure[Input, Result]]
  }

  override def get: Result = throw new Exception("get was called on a ParseFailure")

  override def successful: Boolean = false

  override def toString: String = message

  override def getPartial = partialResult

  override def addDefault[Other >: Result](value: Other): ParseFailure[Input, Other] = partialResult match {
    case _: Some[Result] => this
    case None => ParseFailure(Some(value), remainder, message)
  }

}

trait ParseResult[Input <: ParseInput, +Result] extends ParseResultLike[Input, Result] {
  def map[NewResult](f: Result => NewResult): ParseResult[Input, NewResult]

  def getPartial: Option[Result]
  def get: Result
  def remainder: Input

  def addDefault[Other >: Result](value: Other): ParseResult[Input, Other]
}

case class ParseSuccess[Input <: ParseInput, +Result](result: Result, remainder: Input, biggestFailure: OptionFailure[Result]) extends ParseResult[Input, Result] {
  override def map[NewResult](f: Result => NewResult): ParseSuccess[Input, NewResult] = {
    ParseSuccess(f(result), remainder, biggestFailure.map(f))
  }

  def biggestRealFailure: Option[ParseFailure[Input, Result]] = biggestFailure match {
    case failure: ParseFailure[Input, Result] => Some(failure)
    case _ => None
  }

  def addFailure[Other >: Result](other: OptionFailure[Other]): ParseSuccess[Input, Other] =
    if (biggestFailure.offset > other.offset) this else
      ParseSuccess(result, remainder, other)

  override def get: Result = result

  override def successful: Boolean = true

  override def getPartial = Some(result)

  override def addDefault[Other >: Result](value: Other) = biggestFailure match {
    case NoFailure => this
    case f: ParseFailure[Input, Result] => ParseSuccess(result, remainder, f.addDefault(value))
  }
}

trait OptionFailure[+Result] {
  def offset: Int
  def partialResult: Option[Result]
  def map[NewResult](f: Result => NewResult): OptionFailure[NewResult]
}

object NoFailure extends OptionFailure[Nothing] {
  override def offset: Int = -1

  override def map[NewResult](f: Nothing => NewResult): OptionFailure[NewResult] = this

  override def partialResult: Option[Nothing] = None
}
