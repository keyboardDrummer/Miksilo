package core.gridParser

import core.gridParser.grids.Grid

case class Indent[T](minimumWidth: Int, canBeWider: Boolean, mustParseSomething: Boolean) extends GridParser[T, Unit] {

  override def parse(grid: Grid[T]): ParseResult[Unit] = {
    val whitespace = Some(grid.whitespace)
    var row = 0

    def canParseRow: Boolean = 0.until(minimumWidth).forall(column => grid.get(row, column) == whitespace)
    while(canParseRow) {
      row += 1
    }

    if (row == 0 && minimumWidth > 0) {
      if (mustParseSomething)
        return ParseFailure("no indentation parsed", Location.zero + grid.origin)
      else
        return ParseSuccess(Size.zero, Unit, None)
    }

    var column = minimumWidth
    if (canBeWider) {
      def canParseColumn: Boolean = 0.until(row).forall(columnRow => grid.get(columnRow, column) == whitespace)
      while(canParseColumn) {
        column += 1
      }
    }

    ParseSuccess(Size(column, row), Unit, None)
  }
}
