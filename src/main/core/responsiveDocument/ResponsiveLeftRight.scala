package core.responsiveDocument

import core.document.{LeftRight, Document}

case class ResponsiveLeftRight(left: ResponsiveDocument, right: ResponsiveDocument) extends ResponsiveDocument {
  override def render(preferredWidth: Int): Document = {
    val sizedLeft = left.render(preferredWidth)
    val sizedRight = right.render(preferredWidth - sizedLeft.width)
    new LeftRight(sizedLeft, sizedRight)
  }
}
