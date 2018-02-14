package core.language.node

case class Position(offset: Int /*row: Int, column: Int*/) extends Ordered[Position] {
  override def compare(that: Position): Int = {
    offset.compare(that.offset)
//    val rowResult = row.compareTo(that.row)
//    if (rowResult == 0)
//      column.compareTo(that.column)
//    else
//      rowResult
  }
}
