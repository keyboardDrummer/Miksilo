package core.gridParser

import core.gridParser.grids.Grid

case class Clipped[T, R](size: Size, parser: GridParser[T, R]) extends GridParser[T, R] {

  override def parse(grid: Grid[T]): ParseResult[R] = {
    parser.parse(grid.clip(size))
  }
}
