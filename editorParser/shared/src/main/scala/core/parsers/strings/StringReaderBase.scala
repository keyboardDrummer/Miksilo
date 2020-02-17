package core.parsers.strings

import core.parsers.core.ParseText

abstract class StringReaderBase[Input <: StringReaderBase[Input]](val offset: Int)
  extends StringReaderLike[Input] {

  override def end(array: ParseText) = drop(array, array.length() - offset)

  //val sequence: CharSequence = array

  override def printRange(text: ParseText, end: Input) = text.subSequence(offset, end.offset).toString

  override def atEnd(array: ParseText): Boolean = offset == array.length

  override def head(array: ParseText): Char = array.charAt(offset)

  override def tail(array: ParseText): Input = drop(array, 1)

  override def hashCode(): Int = offset

  override def equals(obj: Any): Boolean = obj match {
    case other: StringReaderBase[Input] => offset == other.offset
    case _ => false
  }

  def print(text: ParseText): String = {
    val position = text.getPosition(offset)
    s"(${position.line}, ${position.character})" +
      text.subSequence(Math.max(0, offset - 10), offset) + " | " + text.subSequence(offset, Math.min(text.length, offset + 10))
  }

  override def toString: String = {
    offset.toString
  }
}
