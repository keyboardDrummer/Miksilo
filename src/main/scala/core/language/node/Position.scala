package core.language.node

case class Position(row: Int, column: Int) extends Ordered[Position] {
  override def compare(that: Position): Int = {
    val rowResult = row.compareTo(that.row)
    if (rowResult == 0)
      column.compareTo(that.column)
    else
      rowResult
  }
}
