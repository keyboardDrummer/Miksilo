package core.responsiveDocument

import core.document.{BlankLine, Document}

object ResponsiveDocument {
  implicit def toResponsive(sized: Document) = new WrappedSizedDocument(sized)
  implicit def text(text: String) = new WrappedSizedDocument(text)
}

case class WrappedSizedDocument(sized: Document) extends ResponsiveDocument {
  override def render(preferredWidth: Int): Document = sized
}

trait ResponsiveDocument {
  def render(preferredWidth: Int) : Document
  def renderString: String = render(Int.MaxValue).render

  def |(right: ResponsiveDocument) = this ~ " " ~ right
  def ~(right: ResponsiveDocument) = new ResponsiveLeftRight(this,right)
  def ^(bottom: ResponsiveDocument) = new ResponsiveTopBottom(this,bottom)
  def ^^(bottom: ResponsiveDocument) = this ^ BlankLine ^ bottom
  def inParenthesis = ResponsiveDocument.text("(") ~ this ~ ")"

  override def toString = renderString
}
