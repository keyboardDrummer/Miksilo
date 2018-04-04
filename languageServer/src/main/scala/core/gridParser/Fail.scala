package core.gridParser

import core.gridParser.grids.Grid

class Fail[T, R](message: String) extends GridParser[T, R] {
  override def parseInner(grid: Grid[T]): ParseResult[R] = {
    ParseFailure(message, Location.zero)
  }
}
