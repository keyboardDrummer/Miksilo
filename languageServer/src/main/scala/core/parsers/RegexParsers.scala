package core.parsers

import scala.util.matching.Regex

trait RegexParsers extends Parsers {
  type Elem = Char
  type Input = StringReader

  implicit class Literal(value: String) extends Parser[String] {
    override def parseInner(inputs: StringReader, cache: ParseState): ParseResult[String] = {
      var index = 0
      val array = inputs.array
      while(index < value.length) {
        val arrayIndex = index + inputs.offset
        if (array.length <= arrayIndex) {
          return ParseFailure(Some(value), inputs, s"expected '$value' but end of source found")
        } else if (array.charAt(arrayIndex) != value.charAt(index)) {
          return ParseFailure(Some(value), inputs.drop(index), s"expected '$value' but found ${array.subSequence(inputs.offset, arrayIndex)}")
        }
        index += 1
      }
      ParseSuccess(value, inputs.drop(value.length), NoFailure)
    }

    override def default: Option[String] = Some(value)
  }

  implicit class RegexFrom(regex: Regex) extends Parser[String] {
    override def parseInner(inputs: StringReader, cache: ParseState): ParseResult[String] = {
      regex.findPrefixMatchOf(new SubSequence(inputs.array, inputs.offset)) match {
        case Some(matched) =>
          ParseSuccess(
            inputs.array.subSequence(inputs.offset, inputs.offset + matched.end).toString,
            inputs.drop(matched.end), NoFailure)        case None =>
          val nextCharacter =
            if (inputs.array.length == inputs.offset) "end of source"
            else inputs.array.charAt(inputs.offset)
          ParseFailure(None, inputs, s"expected '$regex' but found $nextCharacter") // Partial regex matching toevoegen
      }
    }

    override def default: Option[String] = None
  }

  case class StringReader(array: Array[Char], offset: Int = 0) extends InputLike {
    def this(value: String) {
      this(value.toCharArray)
    }

    def drop(amount: Int): StringReader = StringReader(array, offset + amount)

    override def finished: Boolean = offset == array.length
  }
}


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

