package core.parsers.core

import core.parsers.editorParsers.Position

import scala.collection.mutable

object OffsetPointer {
  implicit val ordering: Ordering[OffsetPointer] = new Ordering[OffsetPointer] {
    override def compare(x: OffsetPointer, y: OffsetPointer) = x.offset.compare(y.offset)
  }
}

object StartPointer extends OffsetPointer {
  override def offset = 0
  override def lineCharacter = Position(0, 0)
}

object EndPointer extends OffsetPointer {
  override def offset = Int.MaxValue
  override def lineCharacter = Position(Int.MaxValue, Int.MaxValue)
}

trait OffsetPointer {
  def offset: Int
  def lineCharacter: Position
}

trait TextPointer extends OffsetPointer {
  def safeIncrement: TextPointer = if (atEnd()) this else drop(1)
  def atEnd(): Boolean = offset == length
  def head: Char = charAt(offset)
  def charAt(index: Int): Char
  def length: Int
  def end(): TextPointer = drop(length - offset)
  def charSequence: CharSequence
  def subSequence(from: Int, until: Int): CharSequence
  def drop(amount: Int): TextPointer
  def cache: mutable.HashMap[Any, Any]

  def printRange(end: TextPointer) = subSequence(offset, end.offset).toString
}
