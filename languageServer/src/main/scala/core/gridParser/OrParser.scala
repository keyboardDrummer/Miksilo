package core.gridParser

import core.gridParser.grids.Grid

class OrParser[T, R](first: GridParser[T, R], _second: => GridParser[T, R]) extends GridParser[T, R] {
  lazy val second = _second
  override def parseInner(grid: Grid[T]): ParseResult[R] = {
    val firstResult = first.parse(grid)
    firstResult match {
      case success: ParseSuccess[R] => success
      case failure: ParseFailure[T] => second.parse(grid)
    }
  }
}
