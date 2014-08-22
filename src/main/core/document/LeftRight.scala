package core.document

case class LeftRight(left: Document, right: Document) extends Document {
  val difference = left.height - right.height
  val newLeft = extend(-difference, left)
  val newRight = extend(difference, right)

  override def renderLine(y: Int): String = newLeft.renderLine(y) + newRight.renderLine(y)

  override lazy val height: Int = Math.max(left.height, right.height)

  override lazy val width: Int = left.width + right.width

  def extend(amount: Int, document: Document) =
    if (amount <= 0) document
    else new TopBottom(document, new WhiteSpace(amount, document.height))

}
