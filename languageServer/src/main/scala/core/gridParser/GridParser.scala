package core.gridParser

import scala.util.parsing.combinator.{Parsers, RegexParsers}
import scala.util.parsing.input.CharSequenceReader

trait Grid[T] {
  def isEmpty(row: Int, column: Int): Boolean = get(row, column).fold(true)(_ == whitespace)

  def whitespace: T
  def get(location: Location): Option[T]
  def get(row: Int, column: Int): Option[T]
  def get(offset: Int): T = get(getLocation(offset)).get
  def getLocation(offset: Int): Location
  def getOffset(location: Location): Int
  def getRowWidth(row: Int): Int
  def height: Int
}

case class Location(row: Int, column: Int) {
  def +(size: Size): Location = Location(row + size.height, column + size.width)
}

trait ParseResult[R] {
  def flatMap[R2](f: ParseSuccess[R] => ParseResult[R2]): ParseResult[R2] = {
    ???
  }
}

case class ParseSuccess[R](newLocation: Location, result: R) extends ParseResult[R]
class ParseFailure[R] extends ParseResult[R]

case class Succeed[T, R](value: R) extends GridParser[T, R] {
  override def parse(location: Location, grid: Grid[T]): ParseResult[R] = ParseSuccess(location, value)
}
class Fail[T, R] extends GridParser[T, R] {
  override def parse(location: Location, grid: Grid[T]): ParseResult[R] = {
    new ParseFailure()
  }
}

trait GridParser[T, R] {

  object MyParsers extends Parsers {
    type Elem = T
  }

  def parse(location: Location, grid: Grid[T]): ParseResult[R]
}

case class Size(width: Int, height: Int)

object CharParsers extends RegexParsers {
}

case class Row[R](parser: CharParsers.Parser[R]) extends GridParser[Char, R] {

  override def parse(location: Location, grid: Grid[Char]): ParseResult[R] = {
    val reader = new CharSequenceReader(new CharSequence {
      override def length(): Int = grid.getRowWidth(location.row) - location.column

      override def subSequence(start: Int, end: Int): CharSequence = ???

      override def charAt(index: Int): Char =
        grid.get(location.row, index - location.column).
        getOrElse(throw new IndexOutOfBoundsException())
    })
    val result: CharParsers.ParseResult[R] = parser(reader)
    result match {
      case success: CharParsers.Success[R] =>
        val position = success.next.pos
        val column = position.column
        ParseSuccess(Location(location.row, location.column + column), success.result)
      case f => new ParseFailure
    }
  }
}

case class Wrapped[R](parser: CharParsers.Parser[R]) extends GridParser[Char, R] {

  private val parserWithWhitespace = parser <~ """\s+""".r
  override def parse(location: Location, grid: Grid[Char]): ParseResult[R] = {

    val startingOffset = grid.getOffset(location)
    val reader = new CharSequenceReader(new CharSequence {
      override def length(): Int = {
        location.row.until(grid.height).map(row => grid.getRowWidth(row) - location.column).sum
      }

      override def subSequence(start: Int, end: Int): CharSequence = ???

      override def charAt(index: Int): Char =
        grid.get(startingOffset + index)
    })

    val result: CharParsers.ParseResult[R] = parserWithWhitespace(reader)
    result match {
      case success: CharParsers.Success[R] =>
        val position = success.next.pos
        val column = position.column
        ParseSuccess(Location(location.row, location.column + column), success.result)
      case f => new ParseFailure
    }
}

case class ParseWhitespaceOrEmpty[T](size: Size) extends GridParser[T, Unit] {

  override def parse(location: Location, grid: Grid[T]): ParseResult[Unit] = {
    for(row <- location.row.until(location.row + size.height)) {
      def canParseRow: Boolean = {
        location.column.until(location.column + size.width).forall(column => grid.isEmpty(row, column))
      }
      if (!canParseRow)
        new ParseFailure[Unit]()
    }
    ParseSuccess(location + size, Unit)
  }
}

case class Indent[T](minimumWidth: Int, canBeWider: Boolean) extends GridParser[T, Unit] {

  override def parse(location: Location, grid: Grid[T]): ParseResult[Unit] = {
    val whitespace = Some(grid.whitespace)
    var row = location.row

    def canParseRow: Boolean = {
      location.column.until(location.column + minimumWidth).forall(column => grid.get(row, column) == whitespace)
    }
    while(canParseRow) {
      row += 1
    }

    var column = location.column + minimumWidth
    if (canBeWider) {
      def canParseColumn: Boolean = {
        location.row.until(row).forall(columnRow => grid.get(columnRow, column) == whitespace)
      }

      while(canParseColumn) {
        column += 1
      }
    }

    ParseSuccess(Location(row, column), Unit)
  }
}

case class LeftRight[T, R, R2](left: GridParser[T, R], right: GridParser[T, R2]) extends GridParser[T, (R, R2)] {
  override def parse(leftStart: Location, grid: Grid[T]): ParseResult[(R, R2)] = {
    for {
      leftSuccess: ParseSuccess[R] <- left.parse(leftStart, grid)
      rightStart = Location(leftStart.row, leftSuccess.newLocation.column)
      rightSuccess: ParseSuccess[R2] <- right.parse(rightStart, grid)
      difference = leftSuccess.newLocation.row - rightSuccess.newLocation.row
    } yield if (difference != 0) new ParseFailure() else ParseSuccess(
      rightSuccess.newLocation,
      (leftSuccess.result, rightSuccess.result))
  }
}

case class TopBottom[T, R, R2](top: GridParser[T, R], bottom: GridParser[T, R2]) extends GridParser[T, (R, R2)] {
  override def parse(topStart: Location, grid: Grid[T]): ParseResult[R] = {
    for {
      topSuccess: ParseSuccess[R] <- top.parse(topStart, grid)
      bottomStart = Location(topSuccess.newLocation.row, topStart.column)
      bottomSuccess: ParseSuccess[R2] <- bottom.parse(bottomStart, grid)
      difference = topSuccess.newLocation.column - bottomSuccess.newLocation.column
      remainderRegion = difference.compareTo(0) match {
        case 1 => Some(
          Location(bottomStart.row, bottomSuccess.newLocation.column),
          Size(difference, bottomSuccess.newLocation.row - bottomStart.row))
        case 0 => None
        case -1 =>
          Some(
            Location(topStart.row, topSuccess.newLocation.column),
            Size(-1 * difference, topSuccess.newLocation.row - topStart.row))
      }
      whitespaceSuccess: ParseSuccess[Unit] <- remainderRegion.fold[ParseResult[Unit]](
        ParseSuccess(bottomSuccess.newLocation, Unit))(
        region => ParseWhitespaceOrEmpty(region._2).parse(region._1, grid))
    } yield ParseSuccess(
        Location(bottomSuccess.newLocation.row, whitespaceSuccess.newLocation.column),
        (topSuccess.result, bottomSuccess.result))
  }
}
