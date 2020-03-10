package core.parsers.caching

import core.parsers.core.TextPointer

import scala.collection.mutable

/**
  * A position that includes everything up to and including a particular offset.
  * This position changes if text is added/removed before or at the particular offset.
  */
class InclusivePointer(var leftSide: ExclusivePointer) extends TextPointer {

  override val cache = new mutable.HashMap[Any, Any]

  override def charAt(index: Int) = leftSide.charAt(index)

  override def length = leftSide.length

  override def charSequence = leftSide.charSequence

  override def subSequence(from: Int, until: Int) = leftSide.subSequence(from, until)

  override def drop(amount: Int) = {
    leftSide.drop(amount)
  }

  override def offset = leftSide.offset

  override def lineCharacter = leftSide.lineCharacter
}
