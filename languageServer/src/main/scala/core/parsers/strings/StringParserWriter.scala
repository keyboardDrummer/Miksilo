package core.parsers.strings

import core.language.node.SourceRange
import core.parsers.editorParsers.DefaultCache
import core.parsers.sequences.SequenceParserWriter
import langserver.types.Position

import scala.util.matching.Regex

object ScoredPosition {
  val zero = ScoredPosition(0, Position(0,0))
}

case class ScoredPosition(score: Int, position: Position)

trait StringParserWriter extends SequenceParserWriter {
  type Elem = Char
  type Input <: StringReaderLike

  val consecutiveErrorPowerDiscount = 0.5

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
    def position: Position = scoredPosition.position
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
              val end = remainder.drop(remainder.array.length() - remainder.offset)
              val error = DropError(remainder, end, "end of input")
              ReadyParseResult(parseResult.resultOption, end, parseResult.history.addError(error))
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
              val message = s"expected '$value' but end of source found"
              return singleResult(ReadyParseResult(Some(value), input, new History(MissingInput(input, message, 0.1))))
            } else if (array.charAt(arrayIndex) != value.charAt(index)) {
              val message = s"expected '$value' but found '${array.subSequence(input.offset, arrayIndex + 1)}'"

              return drop(Some(value), input, state, message, result, value)
            }
            index += 1
          }
          val remainder = input.drop(value.length)
          singleResult(ReadyParseResult(Some(value), remainder, new History().addSuccess(remainder)))
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
              val value = input.array.subSequence(input.offset, input.offset + matched.end).toString
              val remainder = input.drop(matched.end)
              singleResult(ReadyParseResult(Some(value), remainder, new History().addSuccess(remainder)))
            case None =>
              if (input.atEnd) {
                val message = s"expected $regexName but found end of source"
                return singleResult(ReadyParseResult(None, input, new History(MissingInput(input, message, 1))))
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

  case class DropError(from: Input, to: Input, expectation: String) extends ParseError {
    def this(from: Input, expectation: String) = this(from, from.drop(1), expectation)

    override def append(next: ParseError): Option[ParseError] = {
      next match {
        case drop: DropError if drop.from == to =>
          Some(DropError(from, drop.to, expectation))
        case _ => None
      }
    }

    override def penalty = {
      val length = to.offset - from.offset
      2.1 - 1.0 / length
    }

    override def message = {
      val found = from.array.subSequence(from.offset, to.offset)
      s"expected $expectation but found '$found'"
    }

    override def range = SourceRange(from.position, to.position)
  }

  def drop[Result](resultOption: Option[Result],
                   input: Input, state: ParseState,
                   errorMessage: String, parse: Parse[Result], expectation: String): SortedParseResults[Result] = {

    val errors = new History(MissingInput(input, errorMessage))
    val withoutDrop = ReadyParseResult(resultOption, input, errors)
    val dropError = new History(new DropError(input, expectation))
    val droppedInput = input.drop(1)
    val dropped = new DelayedParseResult(droppedInput, dropError, () => {
      parse.apply(droppedInput, state).addErrors(dropError)
    })
    new SRCons[Result](withoutDrop, 1, singleResult(dropped))
  }
}
