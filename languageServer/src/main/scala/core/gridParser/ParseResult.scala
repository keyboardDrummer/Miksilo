package core.gridParser

trait ParseResult[R] {
  def flatMap[R2](next: ParseSuccess[R] => ParseResult[R2]): ParseResult[R2]
  def map[R2](mapping: R => R2): ParseResult[R2]
}

case class ParseSuccess[R](size: Size, result: R) extends ParseResult[R] {
  override def flatMap[R2](next: ParseSuccess[R] => ParseResult[R2]): ParseResult[R2] = {
    next(this)
  }

  override def map[R2](mapping: R => R2): ParseResult[R2] = ParseSuccess(size, mapping(result))

  override def toString: String = s"Success(${size.height},${size.width})\n${result.toString}\n"
}

case class ParseFailure[R](message: String, location: Location) extends ParseResult[R] {
  override def flatMap[R2](next: ParseSuccess[R] => ParseResult[R2]): ParseResult[R2] = new ParseFailure[R2](message, location)

  override def map[R2](mapping: R => R2): ParseResult[R2] = ParseFailure(message, location)
}
