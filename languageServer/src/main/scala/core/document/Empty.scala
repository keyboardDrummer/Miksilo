package core.document

object Empty extends Document {
  override def renderLine(y: Int): String = ""

  override val height: Int = 0
  override val width: Int = 0
}
