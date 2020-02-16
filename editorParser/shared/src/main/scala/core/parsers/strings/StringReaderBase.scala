package core.parsers.strings

import core.parsers.core.ParseText
import core.parsers.editorParsers.Position

abstract class StringReaderBase[Input <: StringReaderBase[Input]](val offset: Int, val position: Position)
  extends StringReaderLike[Input] {

  override def end(array: ParseText) = drop(array, array.length() - offset)

  //val sequence: CharSequence = array

  override def printRange(array: ParseText, end: Input) = array.subSequence(offset, end.offset).toString

  override def atEnd(array: ParseText): Boolean = offset == array.length

  override def head(array: ParseText): Char = array.charAt(offset)

  override def tail(array: ParseText): Input = drop(array, 1)

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
