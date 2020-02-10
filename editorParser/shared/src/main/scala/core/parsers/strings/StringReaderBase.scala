package core.parsers.strings

import core.parsers.editorParsers.Position

abstract class StringReaderBase[Input <: StringReaderBase[Input]](val offset: Int, val position: Position)
  extends StringReaderLike[Input] {

  override def end(array: ArrayCharSequence) = drop(array.length() - offset)

  //val sequence: CharSequence = array

  override def printRange(array: ArrayCharSequence, end: Input) = array.subSequence(offset, end.offset).toString

  override def atEnd(array: ArrayCharSequence): Boolean = offset == array.length

  override def head(array: ArrayCharSequence): Char = array.charAt(offset)

  override def tail: Input = drop(1)

  override def hashCode(): Int = offset

  override def equals(obj: Any): Boolean = obj match {
    case other: StringReaderBase[Input] => offset == other.offset
    case _ => false
  }

  def print(array: ArrayCharSequence): String = {
    s"(${position.line}, ${position.character})" +
      array.subSequence(Math.max(0, offset - 10), offset) + " | " + array.subSequence(offset, Math.min(array.length, offset + 10))
  }

  override def toString: String = {
    s"(${position.line}, ${position.character})"
  }
}
