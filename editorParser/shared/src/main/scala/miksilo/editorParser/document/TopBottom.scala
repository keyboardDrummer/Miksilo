package miksilo.editorParser.document

case class TopBottom(top: Document, bottom: Document) extends Document {
  val difference = top.width - bottom.width
  val newTop = appendWhiteSpace(-difference, top)
  val newBottom = appendWhiteSpace(difference, bottom)

  override def renderLine(y: Int): String = {
    val bottomY = y - newTop.height
    if (bottomY < 0)
      newTop.renderLine(y)
    else
      newBottom.renderLine(bottomY)
  }

  override lazy val height: Int = top.height + bottom.height

  override lazy val width: Int = Math.max(top.width, bottom.width)

  def appendWhiteSpace(amount: Int, document: Document) =
    if (amount <= 0) document
    else new LeftRight(document, new WhiteSpace(amount, document.height))
}
