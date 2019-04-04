package core.parsers.strings

import core.parsers.editorParsers.DefaultCache
import core.parsers.sequences.{SequenceInput, SequenceParserWriter}
import langserver.types.Position
import scala.util.matching.Regex

trait StringParserWriter extends SequenceParserWriter {
  type Elem = Char
  type Input <: StringReaderLike

  abstract class StringReaderBase(val array: ArrayCharSequence, val offset: Int, val position: Position) extends StringReaderLike {

    val sequence: CharSequence = array

    override def atEnd: Boolean = offset == array.length

    override def head: Char = array.charAt(offset)

    override def tail: Input = drop(1)

    override def hashCode(): Int = offset

    override def equals(obj: Any): Boolean = obj match {
      case other: StringReaderBase => offset == other.offset
      case _ => false
    }

    override def toString: String = {
      array.subSequence(Math.max(0, offset - 10), offset) + " | " + array.subSequence(offset, Math.min(array.length, offset + 10))
    }
  }

  trait StringReaderLike extends SequenceInput[Input, Char] {
    def position: Position
    def offset: Int
    def array: ArrayCharSequence
    def drop(amount: Int): Input

    def move(increase: Int): Position = {
      var column = position.character
      var row = position.line
      for(index <- offset.until(offset + increase)) {
        if (array.charAt(index) == '\n') {
          row += 1
          column = 0
        } else {
          column += 1
        }
      }
      Position(row, column)
    }
  }

  implicit def literalToExtensions(value: String): ParserExtensions[String] = Literal(value)
  implicit def literal(value: String): Literal = Literal(value)
  implicit def regex(value: Regex): RegexParser = RegexParser(value)

  case class Literal(value: String) extends EditorParserBase[String] {
    override def parseInternal(input: Input, state: ParseState): ParseResult[String] = {
      var index = 0
      val array = input.array
      while(index < value.length) {
        val arrayIndex = index + input.offset
        if (array.length <= arrayIndex) {
          return newFailure(Some(value), input, s"expected '$value' but end of source found")
        } else if (array.charAt(arrayIndex) != value.charAt(index)) {
          return newFailure(Some(value), input.drop(index), s"expected '$value' but found '${array.subSequence(input.offset, arrayIndex + 1)}'")
        }
        index += 1
      }
      newSuccess(value, input.drop(value.length))
    }

    override def getDefault(cache: DefaultCache): Option[String] = Some(value)

    override def children = List.empty
  }

  case class RegexParser(regex: Regex) extends EditorParserBase[String] {
    override def parseInternal(input: Input, state: ParseState): ParseResult[String] = {
      regex.findPrefixMatchOf(new SubSequence(input.array, input.offset)) match {
        case Some(matched) =>
          newSuccess(
            input.array.subSequence(input.offset, input.offset + matched.end).toString,
            input.drop(matched.end))
        case None =>
          val nextCharacter =
            if (input.array.length == input.offset) "end of source"
            else input.array.charAt(input.offset)
          newFailure(input, s"expected '$regex' but found '$nextCharacter'") // Partial regex matching toevoegen
      }
    }

    override def getDefault(cache: DefaultCache): Option[String] = None

    override def children = List.empty
  }
}
