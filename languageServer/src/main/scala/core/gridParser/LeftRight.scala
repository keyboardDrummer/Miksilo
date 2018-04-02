package core.gridParser

import core.gridParser.grids.Grid

class LeftRight[T, R, R2](left: GridParser[T, R], _right: => GridParser[T, R2]) extends GridParser[T, (R, R2)] {
  lazy val right: GridParser[T, R2] = _right

  override def parseInner(leftGrid: Grid[T]): ParseResult[(R, R2)] = {
    left.parse(leftGrid).flatMap(leftSuccess => {
      val rightGrid: Grid[T] = leftGrid.zoomColumn(leftSuccess.size.width).clipHeight(leftSuccess.size.height)
      right.parse(rightGrid).flatMap(rightSuccess => {
        ParseSuccess(rightSuccess.size + Size(leftSuccess.size.width, 0), (leftSuccess.result, rightSuccess.result))
      })
    })
  }
}
