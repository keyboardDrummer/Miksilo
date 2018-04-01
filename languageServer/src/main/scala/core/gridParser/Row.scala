package core.gridParser

import core.gridParser.grids.Grid

import scala.util.parsing.input.CharSequenceReader

case class Row[R](parser: CharParsers.Parser[R]) extends GridParser[Char, R] {

  override def parseInner(grid: Grid[Char]): ParseResult[R] = {
    val reader = new CharSequenceReader(new CharSequence {
      override def length(): Int = if (grid.height > 0) grid.getRowWidth(0) else 0

      override def subSequence(start: Int, end: Int): CharSequence = new SubSequence(this, start ,end)

      override def charAt(index: Int): Char =
        grid.get(0, index).getOrElse(throw new IndexOutOfBoundsException())
    })
    val result: CharParsers.ParseResult[R] = parser(reader)
    val position = result.next.pos
    result match {
      case success: CharParsers.Success[R] =>
        ParseSuccess(Size(position.column - 1, 1), success.result)
      case failure: CharParsers.Failure =>
        ParseFailure(failure.msg, Location(position.line - 1, position.column - 1))
      case error: CharParsers.Error =>
        ParseFailure(error.msg, Location(position.line - 1, position.column - 1))
    }
  }
}
