package core.gridParser

import core.gridParser.grids.Grid

case class Clipped[T, R](size: Size, parser: GridParser[T, R]) extends GridParser[T, R] {

  override def parseInner(grid: Grid[T]): ParseResult[R] = {
    parser.parseInner(grid.clip(size))
  }
}
