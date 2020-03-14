package miksilo.editorParser.document

object Document {
  implicit def fromString(value: String): Document = new Text(value)
}

trait Document {
  def render(trim: Boolean = true): String = {
    0.until(height).map(y => {
      var line = renderLine(y)
      if (trim)
        line = line.reverse.dropWhile(_ == ' ').reverse
      line
    }).mkString(System.lineSeparator)
  }

  def renderLine(y: Int): String

  val height: Int
  val width: Int

  def indent(amount: Int, document: Document) =
    if (amount <= 0) document
    else new LeftRight(new WhiteSpace(amount, document.height), document)

  def ~(right: Document) = new LeftRight(this, right)

  def %(bottom: Document) = new TopBottom(this, bottom)
  
  def %%(bottom: Document) = this % BlankLine % bottom
}
