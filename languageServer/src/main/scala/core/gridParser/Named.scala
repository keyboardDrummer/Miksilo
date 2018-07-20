package core.gridParser

import core.gridParser.grids.Grid

case class Named[T, R](name: String, inner: GridParser[T, R]) extends GridParser[T, R] {
  override def parse(grid: Grid[T]): ParseResult[R] = inner.parse(grid)

  override def toString: String = name
}
