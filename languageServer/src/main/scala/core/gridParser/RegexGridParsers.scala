package core.gridParser

import core.gridParser.grids.{DefaultGrid, Grid}

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.Reader

trait RegexGridParsers extends RegexParsers {
  case class FromLinearParser[R](parser: Parser[R]) extends GridParser[Char, R] {

    override def parse(grid: Grid[Char]): core.gridParser.ParseResult[R] = {

      val defaultGrid = grid.asInstanceOf[DefaultGrid[Char]]
      val reader = new CharGridReader('\n', defaultGrid, LinearGridParsers.getIndexToLocation(grid), 0)
      val result = parser(reader)
      val position = result.next.pos
      result match {
        case success: Success[R] =>
          val height = position.line
          val lastLineWidth = position.column - 1
          val nonLastLineWidths = 0.until(height - 1).map(row => defaultGrid.getRowWidth(row))
          val width = (Seq(lastLineWidth) ++ nonLastLineWidths).max
          //TODO hier nog forceren dat (width - position.column) ook nog geparsed wordt.
          ParseSuccess(Size(width, height), success.result, None)
        case failure: Failure =>
          ParseFailure(failure.msg, Location(position.line - 1, position.column - 1) + grid.origin)
        case error: Error =>
          ParseFailure(error.msg, Location(position.line - 1, position.column - 1) + grid.origin)
      }
    }
  }

  class SubSequence(original: CharSequence, start: Int, end: Int) extends CharSequence {
    override def length(): Int = end - start

    override def subSequence(start: Int, end: Int): CharSequence = new SubSequence(original, this.start + start, this.start + end)

    override def charAt(index: Int): Char = original.charAt(start + index)

    override def toString: String = new String(0.until(length()).map(charAt).toArray)
  }

  class CharGridReader(newLine: Elem, grid: Grid[Elem], indexToLocation: Seq[Location], index: Int)
    extends GridReader[Char](newLine, grid, indexToLocation, index) {

    override def rest: Reader[Char] = new CharGridReader(newLine, grid, indexToLocation, if (atEnd) index else index + 1)

    override val offset: Int = index
    override def source: CharSequence = new CharSequence {
      override def length(): Int = indexToLocation.length

      override def subSequence(start: Int, end: Int): CharSequence = new SubSequence(this, start, end)

      override def charAt(index: Int): Char = {
        grid.get(indexToLocation(index)).getOrElse(newLine)
      }
    }
  }
}