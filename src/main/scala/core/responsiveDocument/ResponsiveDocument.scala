package core.responsiveDocument

import core.document.{BlankLine, Document, WhiteSpace}

object ResponsiveDocument {
  implicit def toResponsive(sized: Document): WrappedSizedDocument = new WrappedSizedDocument(sized)
  implicit def text(text: String): WrappedSizedDocument = new WrappedSizedDocument(text)
}

case class WrappedSizedDocument(sized: Document) extends ResponsiveDocument {
  override def render(preferredWidth: Int): Document = sized

  override def isEmpty: Boolean = sized.width == 0 || sized.height == 0
}

trait ResponsiveDocument {
  def render(preferredWidth: Int) : Document
  def renderString(trim: Boolean = true): String = render(Int.MaxValue).render(trim)
  def isEmpty : Boolean

  def ~~(right: ResponsiveDocument) : ResponsiveDocument = {
    if (isEmpty)
      return right


    if (right.isEmpty)
      return this

    this ~ " " ~ right
  }

  def ~(right: ResponsiveDocument) = {
    new ResponsiveLeftRight(this,right)
  }
  def %(bottom: ResponsiveDocument) = {
    new ResponsiveTopBottom(this,bottom)
  }
  def %%(bottom: ResponsiveDocument): ResponsiveDocument  = {
    if (isEmpty)
      return bottom

    if (bottom.isEmpty)
      return this

    this % BlankLine % bottom
  }
  def inParenthesis = ResponsiveDocument.text("(") ~ this ~ ")"

  def indent(width: Int = 2) = new WhiteSpace(width,0) ~ this

  override def toString = renderString(trim = false)
}
