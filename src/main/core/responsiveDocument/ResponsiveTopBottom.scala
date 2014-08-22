package core.responsiveDocument

import core.document.{TopBottom, Document}

case class ResponsiveTopBottom(top: ResponsiveDocument, bottom: ResponsiveDocument)
  extends ResponsiveDocument {

  override def render(preferredWidth: Int): Document = {
    var renderTop = top.render(preferredWidth)
    val preferredBottomWidth = Math.max(preferredWidth, renderTop.width)
    val renderBottom = bottom.render(preferredBottomWidth)

    if (renderBottom.width > preferredWidth)
      renderTop = top.render(renderBottom.width)

    new TopBottom(renderTop, renderBottom)
  }
}
