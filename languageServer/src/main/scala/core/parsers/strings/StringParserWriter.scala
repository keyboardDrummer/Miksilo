package core.parsers.strings

import java.util.regex.Matcher

import core.parsers.editorParsers.HistoryConstants
import core.parsers.sequences.SequenceParserWriter
import langserver.types.Position

import scala.util.matching.Regex

trait StringParserWriter extends SequenceParserWriter {
  type Elem = Char
  type Input <: StringReaderLike

  val consecutiveErrorPowerDiscount = 0.5

  abstract class StringReaderBase(val array: ArrayCharSequence, val offset: Int, val position: Position)
    extends StringReaderLike {

    override def end = drop(array.length() - offset)

    val sequence: CharSequence = array

    override def printRange(end: Input) = array.subSequence(offset, end.offset).toString

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
    def remaining = array.length() - offset

    def move(increase: Int): Position = {
      var column = position.character
      var row = position.line
      for(index <- offset.until(offset + increase)) {
        val character = array.charAt(index)
        if (character == '\n') {
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
  implicit def regex(value: Regex, regexName: String): RegexParser = RegexParser(value, regexName)

  case class Literal(value: String) extends EditorParserBase[String] with LeafParser[String] {

    override def getParser(recursive: GetParse): Parse[String] = {

      lazy val result: Parse[String] = new Parse[String] {
        def apply(input: Input, state: ParseState): ParseResult[String] = {
          var index = 0
          val array = input.array
          while (index < value.length) {
            val arrayIndex = index + input.offset
            if (array.length <= arrayIndex) {
              return singleResult(ReadyParseResult(Some(value), input,
                new History(new MissingInput(input, s"'$value'", HistoryConstants.endOfSourceInsertion))))
            } else if (array.charAt(arrayIndex) != value.charAt(index)) {
              return singleResult(ReadyParseResult(Some(value), input,
                new History(MissingInput(input, input.drop(index), s"'$value'", HistoryConstants.insertLiteralPenalty))))
            }
            index += 1
          }
          val remainder = input.drop(value.length)
          singleResult(ReadyParseResult(Some(value), remainder, new History().addSuccess(input, remainder, value)))
        }
      }

      result
    }

    override def getMustConsume(cache: ConsumeCache) = value.nonEmpty
  }

  case class RegexParser(regex: Regex, regexName: String) extends EditorParserBase[String] with LeafParser[String] {

    override def getParser(recursive: GetParse): Parse[String] = {

      lazy val result: Parse[String] = new Parse[String] {
        def apply(input: Input, state: ParseState): ParseResult[String] = {
          regex.findPrefixMatchOf(new SubSequence(input.array, input.offset)) match {
            case Some(matched) =>
              val value = input.array.subSequence(input.offset, input.offset + matched.end).toString
              val remainder = input.drop(matched.end)
              singleResult(ReadyParseResult(Some(value), remainder, new History().addSuccess(input, remainder, value)))
            case None =>
              singleResult(ReadyParseResult(None, input, new History(new MissingInput(input, regexName, HistoryConstants.insertRegexPenalty))))
          }
        }
      }

      result
    }

    override def getMustConsume(cache: ConsumeCache) = regex.findFirstIn("").isEmpty
  }
}
