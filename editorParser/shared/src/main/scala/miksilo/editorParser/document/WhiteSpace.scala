package miksilo.editorParser.document

object BlankLine extends WhiteSpace(0,1)

case class WhiteSpace(_width: Int, _height: Int) extends Document {
  override def renderLine(y: Int): String = new String(Array.fill(width)(' '))

  override val height: Int = _height

  override val width: Int = _width
}
