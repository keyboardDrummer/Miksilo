package core.gridParser

import core.gridParser.grids.{DefaultGrid, Grid}

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

object LinearGridParsers {

  def getIndexToLocation[Elem](grid: Grid[Elem]): Seq[Location] = {
    0.until(grid.height).flatMap(row => {
      0.until(grid.width + 1).map(column => Location(row, column))
    })
  }
}

trait LinearGridParsers extends Parsers {

  case class FromLinearParser[R](newLine: Elem, parser: Parser[R]) extends GridParser[Elem, R] {

    override def parse(grid: Grid[Elem]): core.gridParser.ParseResult[R] = {

      val defaultGrid = grid.asInstanceOf[DefaultGrid[Elem]]
      val reader = new GridReader[Elem](newLine, defaultGrid, LinearGridParsers.getIndexToLocation(defaultGrid), 0)
      val result = parser(reader)
      val position = result.next.pos
      val location = Location(position.line - 1, position.column - 1)
      result match {
        case success: Success[R] =>
          ParseSuccess(Size(position.line, position.column), success.result, None)
        case failure: Failure =>
          ParseFailure(failure.msg, grid, location, Some(result.next.offset))
        case failure: Error =>
          ParseFailure(failure.msg, grid, location, Some(result.next.offset))
      }
    }
  }
}

class GridReader[T](newLine: T, grid: Grid[T], indexToLocation: Seq[Location], index: Int) extends Reader[T] {

  override def first: T = grid.get(indexToLocation(index)).getOrElse(newLine)

  override def rest: Reader[T] = new GridReader[T](newLine, grid, indexToLocation, if (atEnd) index else index + 1)

  override lazy val pos: Position = {
    if (index >= indexToLocation.length)
      NoPosition
    else
      new Position {
        override def line: Int = indexToLocation(index).row + 1

        override def column: Int = indexToLocation(index).column + 1

        override protected def lineContents: String = ???
      }
  }

  override def atEnd: Boolean = indexToLocation.length - 1 <= index
}