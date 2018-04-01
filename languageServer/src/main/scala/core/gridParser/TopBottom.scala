package core.gridParser

import core.gridParser.grids.Grid

case class TopBottom[T, R, R2](top: GridParser[T, R], bottom: GridParser[T, R2]) extends GridParser[T, (R, R2)] {
  override def parseInner(topGrid: Grid[T]): ParseResult[(R, R2)] = {
    top.parse(topGrid).flatMap(topSuccess => {
      val bottomGrid = topGrid.zoomRow(topSuccess.size.height)
      bottom.parse(bottomGrid).flatMap(bottomSuccess => {
        val difference = topSuccess.size.width - bottomSuccess.size.width
        val remainderGrid = difference.compareTo(0) match {
          case 1 => bottomGrid.zoomColumn(bottomSuccess.size.width).clipWidth(difference)
          case _ => topGrid.zoomColumn(topSuccess.size.width).clipWidth(-1 * difference)
        }
        ParseWhitespaceOrEmpty(remainderGrid.size).parseInner(remainderGrid).flatMap(_ => {
          ParseSuccess(
            Size(bottomSuccess.size.width + difference, topSuccess.size.height + bottomSuccess.size.height),
            (topSuccess.result, bottomSuccess.result))
          })
        })
      })
  }
}
