package core.gridParser.grids

import core.gridParser.{Location, Size}

trait Grid[T] {
  def isEmpty(row: Int, column: Int): Boolean = get(row, column).fold(true)(_ == whitespace)

  def whitespace: T
  def get(row: Int, column: Int): Option[T]

  def get(location: Location): Option[T] = get(location.row, location.column)
  def getRowWidth(row: Int): Int
  def height: Int
  def width: Int
  def size: Size = Size(width, height)
  def origin: Size
  def areaBeforeStart: Int = origin.width * origin.height + origin.height * size.width + origin.width * size.height

  def zoom(start: Location): Grid[T]
  def zoomColumn(column: Int): Grid[T] = zoom(Location(0, column))
  def zoomRow(row: Int): Grid[T] = zoom(Location(row, 0))
  def clip(size: Size): Grid[T]
  def clipWidth(width: Int): Grid[T] = clip(Size(width, height))
  def clipHeight(height: Int): Grid[T] = clip(Size(width, height))
}