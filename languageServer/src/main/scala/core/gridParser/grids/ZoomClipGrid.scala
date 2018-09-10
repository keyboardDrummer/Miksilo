package core.gridParser.grids

import core.gridParser.{Location, Size}

class ZoomClipGrid[T](zoom: Location, override val size: Size, original: DefaultGrid[T]) extends DefaultGrid[T] {

  override val rows: List[Range] = original.rows.
    slice(zoom.row, zoom.row + size.height).
    map(r => r.slice(zoom.column, zoom.column + size.width))

  override def text: Seq[T] = original.text

  override def whitespace: T = original.whitespace

  override def origin: Size = original.origin + Size(zoom.column, zoom.row)

  override def zoom(extraZoom: Location): Grid[T] = new ZoomClipGrid[T](this.zoom + extraZoom, size - extraZoom, original)

  override def clip(newSize: Size): Grid[T] = new ZoomClipGrid[T](zoom, newSize, original)
}
