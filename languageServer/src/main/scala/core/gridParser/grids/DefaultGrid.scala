package core.gridParser.grids

import core.gridParser.{Location, Size}

trait DefaultGrid[T] extends Grid[T] {
  def text: Seq[T]
  def rows: List[Range]
  lazy val width: Int = (Seq(0) ++ rows.map(r => r.length)).max //TODO gek conflict met ZoomClipGrid size.

  override def get(row: Int, column: Int): Option[T] = {
    if (rows.length <= row)
      return None

    val rowText = rows(row)
    if (rowText.length <= column)
      return None

    Some(text(rowText(column)))
  }

  override def getRowWidth(row: Int): Int = rows(row).length

  override def height: Int = rows.length

  override def zoom(zoom: Location): Grid[T] = new ZoomClipGrid[T](zoom, size - zoom, this)

  override def clip(zoomSize: Size): Grid[T] = new ZoomClipGrid[T](Location.zero, zoomSize, this)
}
