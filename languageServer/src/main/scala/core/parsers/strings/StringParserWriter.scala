package core.parsers.strings

import core.parsers.editorParsers.DefaultCache
import core.parsers.sequences.{SequenceInput, SequenceParserWriter}
import langserver.types.Position
import scala.util.matching.Regex

object ScoredPosition {
  val zero = ScoredPosition(0, Position(0,0))
}

case class ScoredPosition(score: Int, position: Position)

trait StringParserWriter extends SequenceParserWriter {
  type Elem = Char
  type Input <: StringReaderLike

  abstract class StringReaderBase(val array: ArrayCharSequence, val offset: Int, val scoredPosition: ScoredPosition)
    extends StringReaderLike {

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
    def scoredPosition: ScoredPosition
    def position = scoredPosition.position
    def offsetScore = scoredPosition.score
    def offset: Int
    def array: ArrayCharSequence
    def drop(amount: Int): Input
    def remaining = array.length() - offset

    def move(increase: Int): ScoredPosition = {
      var column = position.character
      var row = position.line
      var score = scoredPosition.score
      for(index <- offset.until(offset + increase)) {
        val character = array.charAt(index)
        if (character != '\n' && character != ' ')
          score += 1
        if (character == '\n') {
          row += 1
          column = 0
        } else {
          column += 1
        }
      }
      ScoredPosition(score, Position(row, column))
    }
  }

  override def parseWholeInput[Result](parser: EditorParser[Result], input: Input): ParseWholeResult[Result] = {
    parse(ParseWholeInput(parser), input)
  }

  case class ParseWholeInput[Result](original: Self[Result])
    extends EditorParserBase[Result] with ParserWrapper[Result] {

    override def getParser(recursive: GetParse): Parse[Result] = {
      val parseOriginal = recursive(original)

      new Parse[Result] {
        override def apply(input: Input, state: ParseState) = {
          val result = parseOriginal(input, state)
          result.mapReady(parseResult => {
            val remainder = parseResult.remainder
            if (remainder.atEnd)
              parseResult
            else {
              val missing = remainder.array.subSequence(remainder.offset, remainder.array.length())
              val remainderLength = missing.length()
              val error = ParseError(remainder, s"Found '$missing' instead of end of input", remainderLength)
              ReadyParseResult(parseResult.resultOption, remainder.drop(remainderLength), List(error) ++ parseResult.errors)
            }
          })
        }
      }
    }

    override def getDefault(cache: DefaultCache) = cache(original)
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
              return newFailure(Some(value), input, s"expected '$value' but end of source found")
            } else if (array.charAt(arrayIndex) != value.charAt(index)) {
              val message = s"expected '$value' but found '${array.subSequence(input.offset, arrayIndex + 1)}'"

              //return newFailure(Some(value), input, message)
              return drop(Some(value), input, state, message, result)
            }
            index += 1
          }
          newSuccess(value, input.drop(value.length))
        }
      }

      result
    }


    override def getDefault(cache: DefaultCache): Option[String] = Some(value)

    override def getMustConsume(cache: ConsumeCache) = value.nonEmpty
  }

  case class RegexParser(regex: Regex, regexName: String) extends EditorParserBase[String] with LeafParser[String] {

    override def getParser(recursive: GetParse): Parse[String] = {

      lazy val result: Parse[String] = new Parse[String] {
        def apply(input: Input, state: ParseState): ParseResult[String] = {
          regex.findPrefixMatchOf(new SubSequence(input.array, input.offset)) match {
            case Some(matched) =>
              newSuccess(
                input.array.subSequence(input.offset, input.offset + matched.end).toString,
                input.drop(matched.end))
            case None =>
              if (input.atEnd) {
                val message = s"expected $regexName but found end of source"
                return singleResult(ReadyParseResult(None, input, List(ParseError(input, message, 10))))
              }

              val message = s"expected $regexName but found '${input.array.charAt(input.offset)}'"
              drop(None, input, state, message, result, regexName)
          }
        }
      }

      result
    }

    override def getDefault(cache: DefaultCache): Option[String] = None

    override def getMustConsume(cache: ConsumeCache) = regex.findFirstIn("").isEmpty
  }

  def drop[Result](resultOption: Option[Result],
                   input: Input, state: ParseState,
                   errorMessage: String, parse: Parse[Result]): SortedParseResults[Result] = {

    val errors = List(ParseError(input, errorMessage))
    val withoutDrop = ReadyParseResult(resultOption, input, errors)
    val dropError = List(ParseError(input, s"Dropped '${input.head}'", 4))
    val droppedInput = input.drop(1)
    val dropped = new DelayedParseResult(droppedInput, dropError, () => {
      parse.apply(droppedInput, state).addErrors(dropError)
    })
    new SRCons[Result](withoutDrop, singleResult(dropped))
  }
}
