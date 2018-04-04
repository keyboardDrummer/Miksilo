package core.gridParser

import core.gridParser.grids.Grid

case class ManyVertical[T, R](parser: GridParser[T, R]) extends GridParser[T, List[R]] {
  private val topBottom = new TopBottom[T, R, List[R]](parser, this).map(t => t._1 :: t._2)
  private val result = new OrParser[T, List[R]](topBottom, Succeed[T, List[R]](List.empty))

  override def parseInner(grid: Grid[T]): ParseResult[List[R]] = {
    result.parseInner(grid)
  }
}
