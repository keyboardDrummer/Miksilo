package core.parsers.strings

import core.language.node.Node.PositionOrdering
import core.parsers.editorParsers.{History, ParseError}
import core.parsers.sequences.SequenceParserWriter

case class Position(line: Int, character: Int) extends Ordered[Position] {

  val ordering = Ordering.by[Position, Int](p => p.line).thenComparingInt(p => p.character)
  override def compare(that: Position) = {
    ordering.compare(this, that)
  }
}

case class SourceRange(start: Position, end: Position) {
  def contains(position: Position): Boolean = {
    start <= position && position <= end
  }

  def contains(position: SourceRange): Boolean = {
    start <= position.start && end <= position.end
  }
}

case class TextEdit(range: SourceRange, newText: String)

import scala.util.matching.Regex

trait StringParserWriter extends SequenceParserWriter {
  type Elem = Char
  type Input <: StringReaderLike

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
      s"(${position.line}, ${position.character})" +
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

  val identifierRegex = """[_a-zA-Z][_a-zA-Z0-9]*""".r
  val parseIdentifier = parseRegex(identifierRegex, "identifier")

  implicit def literalToExtensions(value: String): SequenceParserExtensions[String] =
    literalOrKeyword(value)

  implicit def stringToLiteralOrKeyword(value: String): Self[String] = {
    literalOrKeyword(value)
  }

  def literalOrKeyword(value: String, allowDrop: Boolean = true): Self[String] = {
    val isKeyword = identifierRegex.findFirstIn(value).contains(value)
    if (isKeyword)
      if (allowDrop)
        KeywordParser(value)
      else
        ???
    else literal(value, allowDrop = allowDrop)
  }

  def literal(value: String, penalty: Double = History.missingInputPenalty,
              allowDrop: Boolean = true) =
    if (allowDrop) DropParser(Literal(value, penalty)) else Literal(value, penalty)

  case class Literal(value: String, penalty: Double = History.missingInputPenalty) extends ParserBuilderBase[String] with LeafParser[String] {

    override def getParser(recursive: GetParser): Parser[String] = {

      lazy val result: Parser[String] = new Parser[String] {
        def apply(input: Input, state: ParseState): ParseResult[String] = {
          var index = 0
          val array = input.array
          while (index < value.length) {
            val arrayIndex = index + input.offset
            val remainder = input.drop(index)
            val errorHistory = History.error(MissingInput(remainder, value.substring(index), value.substring(index), penalty))
            if (array.length <= arrayIndex) {
              return singleResult(ReadyParseResult(Some(value), remainder, errorHistory))
            } else if (array.charAt(arrayIndex) != value.charAt(index)) {
              return singleResult(ReadyParseResult(Some(value), remainder, errorHistory))
            }
            index += 1
          }
          val remainder = input.drop(value.length)
          singleResult(ReadyParseResult(Some(value), remainder, History.success(input, remainder, value)))
        }
      }
      result

    }

    override def getMustConsume(cache: ConsumeCache) = value.nonEmpty
  }

  /**
    * The purpose of KeywordParser is to parse keyword that is not a prefix of a longer identifier.
    * Don't wrap KeywordParser in a Drop. Since it wraps identifier, it already has a drop.
    */
  case class KeywordParser(value: String) extends ParserBuilderBase[String] with ParserWrapper[String] {
    override def getParser(recursive: GetParser): Parser[String] = {
      val identifierParser = recursive(parseIdentifier)
      (input, state) => {
        identifierParser(input, state).mapReady(ready => {
          if (ready.resultOption.contains(value)) {
            ready
          } else {
            val insertError = MissingInput(input, value, value + " ")
            ReadyParseResult(Some(value), input, History.error(insertError))
          }
        }, uniform = false)
      }
    }

    override def original: Self[String] = parseIdentifier
  }

  trait NextCharError extends ParseError[Input] {
    def to: Input = if (this.from.atEnd) this.from else this.from.drop(1)
    def range = SourceRange(from.position, to.position)
  }

  def parseRegex(regex: Regex, regexName: String,
                 // TODO use the regex to generate a default case.
                 defaultValue: Option[String] = None,
                 score: Double = History.successValue,
                 penaltyOption: Option[Double] = Some(History.missingInputPenalty),
                 allowDrop: Boolean = true) = {
    val initial = RegexParser(regex, regexName, defaultValue, score, penaltyOption)
    if (allowDrop) DropParser(initial) else initial
  }

  case class RegexParser(regex: Regex, regexName: String,
                         // TODO use the regex to generate a default case.
                         defaultValue: Option[String] = None,
                         score: Double = History.successValue,
                         penaltyOption: Option[Double] = Some(History.missingInputPenalty))
    extends ParserBuilderBase[String] with LeafParser[String] {

    override def getParser(recursive: GetParser): Parser[String] = {

      lazy val result: Parser[String] = new Parser[String] {

        def apply(input: Input, state: ParseState): ParseResult[String] = {
          regex.findPrefixMatchOf(new SubSequence(input.array, input.offset)) match {
            case Some(matched) =>
              val value = input.array.subSequence(input.offset, input.offset + matched.end).toString
              val remainder = input.drop(matched.end)
              singleResult(ReadyParseResult(Some(value), remainder, History.success(input, remainder, value, score)))
            case None =>
              penaltyOption.fold[ParseResult[String]](SREmpty)(penalty => {
                val history = History.error(new MissingInput(input, s"<$regexName>", defaultValue.getOrElse(""), penalty))
                singleResult(ReadyParseResult(defaultValue, input, history))
              })

          }
        }
      }

      result
    }

    override def getMustConsume(cache: ConsumeCache) = regex.findFirstIn("").isEmpty
  }
}
