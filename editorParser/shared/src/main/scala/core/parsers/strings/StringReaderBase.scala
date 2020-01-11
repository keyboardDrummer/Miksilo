package core.parsers.strings

import core.parsers.editorParsers.Position

abstract class StringReaderBase[Input <: StringReaderBase[Input]](val array: ArrayCharSequence, val offset: Int, val position: Position)
  extends StringReaderLike[Input] {

  override def end = drop(array.length() - offset)

  val sequence: CharSequence = array

  override def printRange(end: Input) = array.subSequence(offset, end.offset).toString

  override def atEnd: Boolean = offset == array.length

  override def head: Char = array.charAt(offset)

  override def tail: Input = drop(1)

  override def hashCode(): Int = offset

  override def equals(obj: Any): Boolean = obj match {
    case other: StringReaderBase[Input] => offset == other.offset
    case _ => false
  }

  override def toString: String = {
    s"(${position.line}, ${position.character})" +
      array.subSequence(Math.max(0, offset - 10), offset) + " | " + array.subSequence(offset, Math.min(array.length, offset + 10))
  }
}
