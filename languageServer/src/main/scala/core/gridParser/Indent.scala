package core.gridParser

import core.gridParser.grids.Grid

case class Indent[T](minimumWidth: Int, canBeWider: Boolean, mustParseSomething: Boolean) extends GridParser[T, Unit] {

  override def parse(grid: Grid[T]): ParseResult[Unit] = {
    val whitespace = Set(Some(grid.whitespace), None)
    var row = 0


    def canParseRow: Boolean = 0.until(minimumWidth).forall(column => whitespace.contains(grid.get(row, column)))
    while(canParseRow && row < grid.height) {
      row += 1
    }

    if (row == 0 && minimumWidth > 0) {
      if (mustParseSomething)
        return ParseFailure("no indentation parsed", grid, Location.zero)
      else
        return ParseSuccess(Size.zero, Unit, None)
    }

    var column = minimumWidth
    if (canBeWider) {
      def canParseColumn: Boolean = 0.until(row).forall(columnRow => whitespace.contains(grid.get(columnRow, column)))
      while(canParseColumn && column < grid.size.width) {
        column += 1
      }
    }

    ParseSuccess(Size(column, row), Unit, None)
  }
}
