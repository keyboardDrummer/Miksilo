package core.gridParser

import scala.collection.mutable.ArrayBuffer

trait Grid[T] {
  def isEmpty(row: Int, column: Int): Boolean = get(row, column).fold(true)(_ == whitespace)

  def whitespace: T
  def get(row: Int, column: Int): Option[T]

  def get(location: Location): Option[T] = get(location.row, location.column)
  def get(offset: Int): T
  def getRowWidth(row: Int): Int
  def height: Int
  def width: Int
  def size: Size = Size(width, height)

  def zoom(start: Location): Grid[T]
  def zoomColumn(column: Int): Grid[T] = zoom(Location(0, column))
  def zoomRow(row: Int): Grid[T] = zoom(Location(row, 0))
  def clip(size: Size): Grid[T]
  def clipWidth(width: Int): Grid[T] = clip(Size(width, height))
  def clipHeight(height: Int): Grid[T] = clip(Size(width, height))
}

case class GridFromString(value: String) extends Grid[Char] {
  val rows: Array[String] = value.split("\n")
  val rowOffsets: Array[Int] = getLineOffsets
  val width: String = rows.maxBy(r => r.length)

  def getLineOffsets: Array[Int] = {
    val result = ArrayBuffer[Int]()
    result.append(0)
    rows.indices.drop(1).foreach(line => result.append(result.last + 1 + rows(line - 1).length))

    result.toArray
  }

  override def whitespace: Char = ' '

  override def get(row: Int, column: Int): Option[Char] = {
    if (rows.length <= row)
      return None

    val rowText = rows(row)
    if (rowText.length <= column)
      return None

    Some(rowText.charAt(column))
  }

  override def getRowWidth(row: Int): Int = rows(row).length

  override def get(offset: Int): Char = value.charAt(offset)

  override def height: Int = rows.length

  override def zoom(start: Location): Grid[Char] = ???

  override def clip(size: Size): Grid[Char] = ???
}

class EmptyGrid[T] extends Grid[T] {
}