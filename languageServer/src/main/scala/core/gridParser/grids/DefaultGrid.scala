package core.gridParser.grids

import java.util

import core.gridParser.{Location, Size}

trait DefaultGrid[T] extends Grid[T] {
  def text: Seq[T]
  def rows: List[Range]
  private lazy val rowOffsets = rows.map(r => r.head).toArray
  lazy val width: Int = (Seq(0) ++ rows.map(r => r.length)).max

  override def get(row: Int, column: Int): Option[T] = {
    if (rows.length <= row)
      return None

    val rowText = rows(row)
    if (rowText.length <= column)
      return None

    Some(text(rowText(column)))
  }

  override def getRowWidth(row: Int): Int = rows(row).length

  override def get(offset: Int): T = {
    val searchResult = util.Arrays.binarySearch(rowOffsets, offset)
    val resultOption = if (searchResult >= 0) {
      get(searchResult, 0)
    } else {
      val row = searchResult * -1 - 1
      get(row, offset - rowOffsets(row))
    }
    resultOption.get
  }

  override def height: Int = rows.length

  override def zoom(zoom: Location): Grid[T] = new ZoomClipGrid[T](zoom, size - zoom, this)

  override def clip(zoomSize: Size): Grid[T] = new ZoomClipGrid[T](Location.zero, zoomSize, this)
}
