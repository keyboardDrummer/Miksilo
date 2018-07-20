package core.gridParser

import core.gridParser.grids.Grid

case class Succeed[T, R](value: R) extends GridParser[T, R] {
  override def parse(grid: Grid[T]): ParseResult[R] = ParseSuccess(Size.zero, value, None)
}
