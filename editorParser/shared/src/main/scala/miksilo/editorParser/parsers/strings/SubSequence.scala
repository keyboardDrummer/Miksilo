package miksilo.editorParser.parsers.strings

class SubSequence(original: CharSequence, start: Int, val length: Int) extends CharSequence {
  def this(s: CharSequence, start: Int) = this(s, start, s.length - start)

  def charAt(index: Int): Char =
    if (index >= 0 && index < length) original.charAt(start + index) else throw new IndexOutOfBoundsException(s"index: $index, length: $length")

  def subSequence(_start: Int, _end: Int): SubSequence = {
    if (_start < 0 || _end < 0 || _end > length || _start > _end)
      throw new IndexOutOfBoundsException(s"start: ${_start}, end: ${_end}, length: $length")

    new SubSequence(original, start + _start, _end - _start)
  }

  override def toString: String = original.subSequence(start, start + length).toString
}
