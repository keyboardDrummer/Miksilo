package core.gridParser

import core.gridParser.grids.{DefaultGrid, Grid}

import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.input.Reader

trait RegexGridParsers extends RegexParsers {
  case class FromLinearParser[R](parser: Parser[R]) extends GridParser[Char, R] {

    override def parseInner(grid: Grid[Char]): core.gridParser.ParseResult[R] = {

      val defaultGrid = grid.asInstanceOf[DefaultGrid[Char]]
      val reader = new CharGridReader(defaultGrid, Search.getOffset(defaultGrid))
      val result = parser(reader)
      val position = result.next.pos
      result match {
        case success: Success[R] =>
          val height = position.line - grid.origin.height
          val lastLineWidth = position.column - grid.origin.width - 1
          val nonLastLineWidths = 0.until(height - 1).map(row => defaultGrid.getRowWidth(row))
          val width = (Seq(lastLineWidth) ++ nonLastLineWidths).max
          //TODO hier nog forceren dat (width - position.column) ook nog geparsed wordt.
          ParseSuccess(Size(width, height), success.result)
        case failure: Failure =>
          ParseFailure(failure.msg, Location(position.line - 1, position.column - 1))
        case error: Error =>
          ParseFailure(error.msg, Location(position.line - 1, position.column - 1))
      }
    }
  }

  class SubSequence(original: CharSequence, start: Int, end: Int) extends CharSequence {
    override def length(): Int = end - start

    override def subSequence(start: Int, end: Int): CharSequence = new SubSequence(original, this.start + start, this.start + end)

    override def charAt(index: Int): Char = original.charAt(start + index)

    override def toString: String = new String(0.until(length()).map(charAt).toArray)
  }

  class CharGridReader(grid: DefaultGrid[Char], override val offset: Int) extends GridReader[Char](grid, offset) {

    override def rest: Reader[Char] = new CharGridReader(grid, offset + 1)

    override def source: CharSequence = new CharSequence {
      override def length(): Int = grid.text.length

      override def subSequence(start: Int, end: Int): CharSequence = new SubSequence(this, start, end)

      override def charAt(index: Int): Char = {
        grid.text(index)
      }
    }
  }
}