package core.gridParser

import core.gridParser.grids.Grid

import scala.util.parsing.input.CharSequenceReader

class SubSequence(origin: CharSequence, start: Int, end: Int) extends CharSequence {
  override def length(): Int = end - start

  override def subSequence(start: Int, end: Int): CharSequence = new SubSequence(origin, this.start + start, this.start + end)

  override def charAt(index: Int): Char = origin.charAt(start + index)

  override def toString: String = new String(0.until(length()).map(charAt).toArray)
}

case class Wrapped[R](parser: CharParsers.Parser[R]) extends GridParser[Char, R] {

  private val parserWithWhitespace = parser <~ """\s+""".r
  override def parseInner(grid: Grid[Char]): ParseResult[R] = {

    val reader = new CharSequenceReader(new CharSequence {
      override def length(): Int = {
        0.until(grid.height).map(row => grid.getRowWidth(row)).sum
      }

      override def subSequence(start: Int, end: Int): CharSequence = new SubSequence(this, start, end)

      override def charAt(index: Int): Char =
        grid.get(index)
    })

    val result: CharParsers.ParseResult[R] = parserWithWhitespace(reader)
    val position = result.next.pos
    result match {
      case success: CharParsers.Success[R] =>
        ParseSuccess(Size(position.line, position.column), success.result)
      case failure: CharParsers.Failure =>
        ParseFailure(failure.msg, Location(position.line - 1, position.column - 1))
      case error: CharParsers.Error =>
        ParseFailure(error.msg, Location(position.line - 1, position.column - 1))
    }
  }
}
