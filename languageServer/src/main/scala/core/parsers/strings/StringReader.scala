package core.parsers.strings

import core.parsers.SequenceInput

import scala.util.parsing.input.OffsetPosition

case class StringReader(array: Array[Char], offset: Int = 0) extends SequenceInput[StringReader, Char] {
  def this(value: String) {
    this(value.toCharArray)
  }

  def drop(amount: Int): StringReader = StringReader(array, offset + amount)
  def position = OffsetPosition(array, offset)

  override def finished: Boolean = offset == array.length

  override def atEnd: Boolean = array.length == offset

  override def head: Char = array(offset)

  override def tail: StringReader = drop(1)
}
