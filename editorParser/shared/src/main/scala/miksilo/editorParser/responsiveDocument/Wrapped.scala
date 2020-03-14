package miksilo.editorParser.responsiveDocument

import miksilo.editorParser.document.{Document, Empty, LeftRight, TopBottom}

case class Wrapped(items: Seq[ResponsiveDocument]) extends ResponsiveDocument {
  override def render(preferredWidth: Int): Document = {
    var lineWidth = preferredWidth
    var result: Document = Empty
    var line: Document = Empty
    for(item <- items)
    {
      val remainingWidth = lineWidth - line.width
      val sizedItem = item.render(remainingWidth)
      lineWidth = Math.max(lineWidth, sizedItem.width)
      if (sizedItem.width > remainingWidth) {
        result = new TopBottom(result, line)
        line = sizedItem
      } else
      {
        line = new LeftRight(line, sizedItem)
      }
    }
    result
  }

  override def isEmpty: Boolean = items.forall(item => item.isEmpty)
}
