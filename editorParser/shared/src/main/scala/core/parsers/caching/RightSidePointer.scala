package core.parsers.caching

import core.parsers.core.TextPointer

import scala.collection.mutable

class RightSidePointer(var leftSide: LeftSidePointer) extends TextPointer {

  override val cache = new mutable.HashMap[Any, Any]

  override def charAt(index: Int) = leftSide.charAt(index)

  override def length = leftSide.length

  override def charSequence = leftSide.charSequence

  override def subSequence(from: Int, until: Int) = leftSide.subSequence(from, until)

  override def drop(amount: Int) = {
    if (amount == 0)
      throw new Exception("what?")
    leftSide.drop(amount)
  }

  override def offset = leftSide.offset

  override def lineCharacter = leftSide.lineCharacter
}
