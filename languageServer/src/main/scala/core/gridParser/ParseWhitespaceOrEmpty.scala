package core.gridParser

import core.gridParser.grids.Grid

case class ParseWhitespaceOrEmpty[T](size: Size) extends GridParser[T, Unit] {

  override def parse(grid: Grid[T]): ParseResult[Unit] = {
    for(row <- 0.until(size.height)) {
      val columnFailure = 0.until(size.width).find(column => grid.isEmpty(row, column))
      if (columnFailure.nonEmpty)
        new ParseFailure[Unit]("No whitespace or empty found", Location(row, columnFailure.get))
    }
    ParseSuccess(size, Unit, None)
  }
}
