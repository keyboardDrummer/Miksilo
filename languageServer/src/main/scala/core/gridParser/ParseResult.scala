package core.gridParser

trait ParseResult[R] {
  def flatMap[R2](next: ParseSuccess[R] => ParseResult[R2]): ParseResult[R2]
  def map[R2](mapping: R => R2): ParseResult[R2]
}

case class ParseSuccess[R](size: Size, result: R, biggestFailure: Option[ParseFailure[R]] = None)
  extends ParseResult[R] {
  override def flatMap[R2](next: ParseSuccess[R] => ParseResult[R2]): ParseResult[R2] = {
    next(this) match {
      case success: ParseSuccess[R2] =>
        ParseSuccess(success.size, success.result, (biggestFailure, success.biggestFailure) match {
          case (Some(ff), Some(sf)) => Some(ff.getBiggestFailure(sf))
          case (f, None) => f.asInstanceOf[Option[ParseFailure[R2]]]
          case (None, s) => s
          case _ => None
        })
      case failure: ParseFailure[R2] => failure //TODO moet ik hier nog naar success.biggestFailure kijken?
    }
  }

  override def map[R2](mapping: R => R2): ParseResult[R2] = ParseSuccess(size, mapping(result),
    biggestFailure.asInstanceOf[Option[ParseFailure[R2]]])

  override def toString: String = s"Success(${size.height},${size.width})\n${result.toString}\n"
}

case class ParseFailure[R](message: String, location: Location) extends ParseResult[R] {
  System.out.append(" ")

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

}
