package core.gridParser

import core.gridParser.grids.Grid

trait ParseResult[R] {
  def flatMap[R2](next: ParseSuccess[R] => ParseResult[R2]): ParseResult[R2]
  def map[R2](mapping: R => R2): ParseResult[R2]
  def addFailure[R2](failure: ParseFailure[R2]): ParseResult[R]
  val testProperties: Any
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

  override lazy val testProperties: Any = (size, result)
}

case class ParseFailure[R](message: String, inside: Grid[_], location: Location, parsedOverride: Option[Int] = None)
  extends ParseResult[R] {

  lazy val testProperties = (message, absoluteLocation)
  val absoluteLocation = location + inside.origin
  lazy val parsed = parsedOverride.getOrElse(location.row * location.column)
  val totalParsed = inside.areaBeforeStart + parsed
  override def flatMap[R2](next: ParseSuccess[R] => ParseResult[R2]): ParseResult[R2] =
    new ParseFailure[R2](message, inside, location, parsedOverride)

  override def map[R2](mapping: R => R2): ParseResult[R2] = ParseFailure(message, inside, location, parsedOverride)

  def getBiggestFailure[R2](other: ParseFailure[R2]): ParseFailure[R2] = {
    if (totalParsed > other.totalParsed)
      this.asInstanceOf[ParseFailure[R2]]
    else
      other
  }

  override def addFailure[R2](failure: ParseFailure[R2]): ParseResult[R] = this.getBiggestFailure(failure.asInstanceOf[ParseFailure[R]])

  override def toString = s"$message at $absoluteLocation"
}
