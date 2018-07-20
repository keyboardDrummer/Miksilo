package core.gridParser

trait ParseResult[R] {
  def flatMap[R2](next: ParseSuccess[R] => ParseResult[R2]): ParseResult[R2]
  def map[R2](mapping: R => R2): ParseResult[R2]
  def addFailure[R2](failure: ParseFailure[R2]): ParseResult[R]
}

case class ParseSuccess[R](size: Size, result: R, biggestFailure: Option[ParseFailure[R]] = None)
  extends ParseResult[R] {
  override def flatMap[R2](next: ParseSuccess[R] => ParseResult[R2]): ParseResult[R2] = {
    val result = next(this)
    biggestFailure.fold(result)(f => result.addFailure(f))
  }

  override def map[R2](mapping: R => R2): ParseResult[R2] = ParseSuccess(size, mapping(result),
    biggestFailure.asInstanceOf[Option[ParseFailure[R2]]])

  override def toString: String = s"Success(${size.height},${size.width})\n${result.toString}\n"

  override def addFailure[R2](failure: ParseFailure[R2]): ParseResult[R] = {
    val castFailure = failure.asInstanceOf[ParseFailure[R]]
    ParseSuccess[R](size, result, Some(this.biggestFailure.fold(castFailure)(f => f.getBiggestFailure[R](castFailure))))
  }
}

case class ParseFailure[R](message: String, location: Location) extends ParseResult[R] {

  override def flatMap[R2](next: ParseSuccess[R] => ParseResult[R2]): ParseResult[R2] = new ParseFailure[R2](message, location)

  override def map[R2](mapping: R => R2): ParseResult[R2] = ParseFailure(message, location)

  def getBiggestFailure[R2](other: ParseFailure[R2]): ParseFailure[R2] = {
    if (this.location.row > other.location.row) {
      return this.asInstanceOf[ParseFailure[R2]]
    }

    if (other.location.row > this.location.row) {
      return other
    }

    if (this.location.column > other.location.column) {
      return this.asInstanceOf[ParseFailure[R2]]
    }

    other
  }

  override def addFailure[R2](failure: ParseFailure[R2]): ParseResult[R] = this.getBiggestFailure(failure.asInstanceOf[ParseFailure[R]])
}
