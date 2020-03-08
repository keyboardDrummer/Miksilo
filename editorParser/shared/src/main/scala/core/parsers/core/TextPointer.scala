package core.parsers.core

import core.parsers.editorParsers.Position

import scala.collection.mutable

trait TextPointer {
  def safeIncrement: TextPointer = if (atEnd()) this else drop(1)
  def atEnd(): Boolean = offset == length
  def head: Char = charAt(offset)
  def charAt(index: Int): Char
  def length: Int
  def end(): TextPointer = drop(length - offset)
  def charSequence: CharSequence
  def subSequence(from: Int, until: Int): CharSequence
  def lineCharacter: Position
  def drop(amount: Int): TextPointer
  def offset: Int
  def toPosition(text: ParseText): Position = text.getPosition(offset)
  def cache: mutable.HashMap[Any, Any]
  def cache_=(value: mutable.HashMap[Any, Any]): Unit

  def printRange(end: TextPointer) = subSequence(offset, end.offset).toString
}
