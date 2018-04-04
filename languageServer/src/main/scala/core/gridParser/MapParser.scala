package core.gridParser

import core.gridParser.grids.Grid

case class MapParser[T, R, R2](parser: GridParser[T, R], mapping: R => R2) extends GridParser[T, R2] {
  override def parseInner(grid: Grid[T]): ParseResult[R2] = parser.parseInner(grid).map(mapping)
}
