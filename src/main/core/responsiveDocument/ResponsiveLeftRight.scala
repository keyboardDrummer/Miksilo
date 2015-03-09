package core.responsiveDocument

import core.document.{Document, LeftRight}

case class ResponsiveLeftRight(left: ResponsiveDocument, right: ResponsiveDocument) extends ResponsiveDocument {
  override def render(preferredWidth: Int): Document = {
    val sizedLeft = left.render(preferredWidth)
    val sizedRight = right.render(preferredWidth - sizedLeft.width)
    new LeftRight(sizedLeft, sizedRight)
  }

  override def isEmpty: Boolean = left.isEmpty && right.isEmpty
}
