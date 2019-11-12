package core.parsers


trait DocumentNode {
  def getValue(position: Int): Option[String]
  def insertNode(position: Int, node: DocumentNode): DocumentNode
  def insertValue(position: Int, length: Int, value: String): DocumentNode =
    insertNode(position, ParentNode(0, length, value, NoNode, NoNode))

  def changeRange(position: Int, oldLength: Int, newLength: Int): DocumentNode = {
    removeText(position, oldLength).insertText(position, newLength)
  }

  def insertText(from: Int, length: Int): DocumentNode
  def removeText(position: Int, length: Int): DocumentNode
}

case class ParentNode(myAndRightOffset: Int, length: Int, value: String, left: DocumentNode, right: DocumentNode) extends DocumentNode {

  override def insertNode(position: Int, node: DocumentNode) = {
    val relativePosition = position - myAndRightOffset
    if (relativePosition == 0)
      throw new Exception("Node already exists")
    else if (relativePosition < 0)
      ParentNode(myAndRightOffset, length, value, left.insertNode(position, node), right)
    else
      ParentNode(myAndRightOffset, length, value, left, right.insertNode(relativePosition, node))
  }

  override def insertText(from: Int, insertLength: Int) = {
    val relativeFrom = from - myAndRightOffset
    if (relativeFrom <= 0) {
      // To the left of this node
      ParentNode(myAndRightOffset + insertLength, length, value, left.insertText(from, insertLength), right)
    } else if (relativeFrom >= length) {
      // To the right of this node
      ParentNode(myAndRightOffset, length, value, left, right.insertText(from, insertLength))
    } else {
      // This node disappears
      val newLeft = left.insertText(from, insertLength)
      val newRight = right.insertText(relativeFrom, insertLength)
      newLeft.insertNode(myAndRightOffset, newRight)
    }
  }

  override def removeText(position: Int, length: Int) = {
    val relativePosition = position - myAndRightOffset
    if (relativePosition < 0) {
      val end = position + length - 1
      val newLeft = left.removeText(position, length)
      if (end >= myAndRightOffset) {
        // This node disappears
        newLeft.insertNode(myAndRightOffset, right.removeText(0, length + relativePosition))
      } else {
        // To the left of this node
        ParentNode(myAndRightOffset, length, value, newLeft, right)
      }
    } else if (relativePosition == 0) {
      // This node disappears
      left.insertNode(myAndRightOffset, right.removeText(relativePosition, length))
    } else
      ParentNode(myAndRightOffset, length, this.value, left, right.removeText(relativePosition, length))
  }

  override def getValue(position: Int) = {
    val relativePosition = position - myAndRightOffset
    if (relativePosition < 0)
      left.getValue(position)
    else if (relativePosition == 0)
      Some(value)
    else
      right.getValue(relativePosition)
  }
}

object NoNode extends DocumentNode {

  override def removeText(position: Int, length: Int) = this

  override def insertNode(position: Int, node: DocumentNode) = node.insertText(0, position)

  override def insertText(from: Int, until: Int) = this

  override def getValue(position: Int) = None
}