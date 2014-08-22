package core.document

object Document {
  implicit def fromString(value: String): Document = new Text(value)
}

trait Document {
  def render: String = {
    0.until(height).map(y => renderLine(y)).mkString("\n")
  }

  def renderLine(y: Int): String

  val height: Int
  val width: Int

  def indent(amount: Int, document: Document) =
    if (amount <= 0) document
    else new LeftRight(new WhiteSpace(amount, document.height), document)

  def ~(right: Document) = new LeftRight(this, right)

  def ^(bottom: Document) = new TopBottom(this, bottom)

  def ^^(bottom: Document) = this ^ BlankLine ^ bottom
}
