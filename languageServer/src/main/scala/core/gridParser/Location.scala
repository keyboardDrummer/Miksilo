package core.gridParser

object Location {
  val zero = Location(0,0)
}

case class Location(row: Int, column: Int) {
  def +(size: Size): Location = Location(row + size.height, column + size.width)
  def +(size: Location): Location = Location(row + size.row, column + size.column)
}
