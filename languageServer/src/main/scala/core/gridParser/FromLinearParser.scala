package core.gridParser

import core.gridParser.grids.{DefaultGrid, Grid}

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{Position, Reader}

trait LinearGridParsers extends Parsers {
  case class FromLinearParser[R](parser: Parser[R]) extends GridParser[Elem, R] {

    override def parseInner(grid: Grid[Elem]): core.gridParser.ParseResult[R] = {

      val defaultGrid = grid.asInstanceOf[DefaultGrid[Elem]]
      val reader = new GridReader[Elem](defaultGrid, Search.getOffset(defaultGrid))
      val result = parser(reader)
      val position = result.next.pos
      result match {
        case success: Success[R] =>
          ParseSuccess(Size(position.line, position.column), success.result)
        case failure: Failure =>
          ParseFailure(failure.msg, Location(position.line - 1, position.column - 1))
        case error: Error =>
          ParseFailure(error.msg, Location(position.line - 1, position.column - 1))
      }
    }
  }
}

object Search {
  def getOffset[T](grid: DefaultGrid[T]): Int = {
    grid.rows.flatMap(row => row.headOption.toSeq).headOption.getOrElse(grid.text.length)
  }

  def binarySearch[T](elements: Seq[T], key: T)(implicit ordering: Ordering[T]): Int = {
    var low = 0
    var high = elements.length - 1
    while (low <= high) {
      val mid = (low + high) >>> 1
      val midVal = elements(mid)
      if (ordering.lt(midVal, key))
        low = mid + 1
      else if (ordering.gt(midVal, key))
        high = mid - 1
      else
        return mid // key found
    }
    -(low + 1) // key not found.
  }
}
class GridReader[T](grid: DefaultGrid[T], offset: Int) extends Reader[T] {
  override def first: T = grid.text(offset)

  override def rest: Reader[T] = new GridReader[T](grid, offset + 1)

  override lazy val pos: Position = new Position {
    lazy val lineAndColumn: (Int, Int) = {
      val nonEmptyRowOffsetsWithOriginalIndex: Seq[(Int, Int)] = grid.rows.zipWithIndex.flatMap(row => row._1.headOption.toSeq.map(x => (x, row._2)))
      val searchResult = Search.binarySearch(nonEmptyRowOffsetsWithOriginalIndex.map(t => t._1), offset)
      if (searchResult >= 0) {
        (nonEmptyRowOffsetsWithOriginalIndex(searchResult)._2, 0)
      } else {
        val insertIndex = searchResult * -1 - 1
        val nonEmptyRowIndex = insertIndex - 1
        if (nonEmptyRowIndex == -1)
          (grid.origin.height + 0, grid.origin.width + 0)
        else {
          val (rowOffset, row) = nonEmptyRowOffsetsWithOriginalIndex(nonEmptyRowIndex)
          (grid.origin.height + row + 1, grid.origin.width + offset - rowOffset + 1)
        }
      }
    }

    override def line: Int = lineAndColumn._1

    override def column: Int = lineAndColumn._2

    override protected def lineContents: String = ???
  }

  override def atEnd: Boolean = grid.text.length == offset
}