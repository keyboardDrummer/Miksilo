package core.parsers.strings

import core.parsers.editorParsers.DefaultCache
import core.parsers.sequences.{SequenceInput, SequenceParserWriter}

import scala.util.matching.Regex

trait StringParserWriter extends SequenceParserWriter {
  type Elem = Char
  type Input <: StringReaderLike

  trait StringReaderLike extends SequenceInput[Input, Char] {
    def offset: Int
    def array: ArrayCharSequence
    def drop(amount: Int): Input
  }

  implicit def literalToExtensions(value: String): ParserExtensions[String] = Literal(value)
  implicit def literal(value: String): Literal = Literal(value)
  implicit def regex(value: Regex): RegexParser = RegexParser(value)

  case class Literal(value: String) extends EditorParser[String] {
    override def parseInternal(inputs: Input, state: ParseStateLike): ParseResult[String] = {
      var index = 0
      val array = inputs.array
      while(index < value.length) {
        val arrayIndex = index + inputs.offset
        if (array.length <= arrayIndex) {
          return newFailure(Some(value), inputs, s"expected '$value' but end of source found")
        } else if (array.charAt(arrayIndex) != value.charAt(index)) {
          return newFailure(Some(value), inputs.drop(index), s"expected '$value' but found '${array.subSequence(inputs.offset, arrayIndex + 1)}'")
        }
        index += 1
      }
      newSuccess(value, inputs.drop(value.length))
    }

    override def getDefault(cache: DefaultCache): Option[String] = Some(value)
  }

  case class RegexParser(regex: Regex) extends EditorParser[String] {
    override def parseInternal(input: Input, state: ParseStateLike): ParseResult[String] = {
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
  }
}
