package core.gridParser

import core.gridParser.grids.Grid

class OrParser[T, R](first: GridParser[T, R], _second: => GridParser[T, R]) extends GridParser[T, R] {
  lazy val second = _second
  override def parse(grid: Grid[T]): ParseResult[R] = {
    val firstResult = first.parse(grid)
    firstResult match {
      case success: ParseSuccess[R] => success
      case failure: ParseFailure[R] => second.parse(grid).addFailure(failure)
    }
  }

  def getBiggestFailure(first: ParseFailure[R], second: ParseFailure[R]): ParseFailure[R] = {
    if (first.location.row > second.location.row) {
      return first
    }

    if (second.location.row > first.location.row) {
      return second
    }

    if (first.location.column > second.location.column) {
      return first
    }

    second
  }
}

//Always return the biggest failure.