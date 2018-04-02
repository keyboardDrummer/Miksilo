package core.gridParser

import core.gridParser.grids.Grid

case class Indent[T](minimumWidth: Int, canBeWider: Boolean) extends GridParser[T, Unit] {

  override def parseInner(grid: Grid[T]): ParseResult[Unit] = {
    val whitespace = Some(grid.whitespace)
    var row = 0

    def canParseRow: Boolean = 0.until(minimumWidth).forall(column => grid.get(row, column) == whitespace)
    while(canParseRow) {
      row += 1
    }

    var column = minimumWidth
    if (canBeWider) {
      def canParseColumn: Boolean = 0.until(row).forall(columnRow => grid.get(columnRow, column) == whitespace)
      while(canParseColumn) {
        column += 1
      }
    }

    ParseSuccess(Size(column, row), Unit)
  }
}
