package core.gridParser.grids

import core.gridParser.Size

case class GridFromString(content: String) extends DefaultGrid[Char] {

  def getRows: List[Range] = {
    val newLineIndices = Seq(-1) ++ "\n".r.findAllMatchIn(content).map(m => m.start) ++ Seq(content.length)
    newLineIndices.zip(newLineIndices.drop(1)).map(t => Range(t._1 + 1, t._2)).toList
  }

  override def whitespace: Char = ' '

  override val rows: List[Range] = getRows

  override def origin: Size = Size.zero

  override def text: Seq[Char] = new Seq[Char] {
    override def length: Int = content.length

    override def apply(idx: Int): Char = content.charAt(idx)

    override def iterator: Iterator[Char] = content.toIterator
  }
}
