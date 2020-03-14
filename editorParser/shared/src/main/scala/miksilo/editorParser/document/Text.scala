package miksilo.editorParser.document

case class Text(text: String) extends Document {
  override def renderLine(y: Int): String = text

  override val height: Int = 1
  override val width: Int = text.length
}
