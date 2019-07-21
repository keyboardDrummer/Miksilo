package core.parsers.strings

import core.parsers.editorParsers.{FlawedHistory, History, ParseError, SpotlessHistory}
import core.parsers.sequences.SequenceParserWriter
import languageServer.{Position, SourceRange}

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
  val identifier = parseRegex(identifierRegex, "identifier")

  implicit def literalToExtensions(value: String): SequenceParserExtensions[String] = Literal(value)

  implicit def stringToLiteralOrKeyword(value: String): Self[String] = {
    literalOrKeyword(value)
  }

  def literalOrKeyword(value: String, allowDrop: Boolean = true): Self[String] = {
    val isKeyword = identifierRegex.findFirstIn(value).contains(value)
    if (isKeyword) KeywordParser(value) else literal(value, allowDrop = allowDrop)
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
            if (array.length <= arrayIndex) {
              return singleResult(ReadyParseResult(Some(value), input,
                History.error(new MissingInput(input, value, value.substring(index), penalty))))
            } else if (array.charAt(arrayIndex) != value.charAt(index)) {
              return singleResult(ReadyParseResult(Some(value), input,
                History.error(MissingInput(input, input.drop(index + 1), value, value.substring(index), penalty))))
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
    * Don't wrap KeywordParser in a Drop. Since it wraps identifier, it already has a drop.
    * What's the point of letting KeywordParser use identifier instead of Literal?
    * Using Literal would be much simpler.
    * The situation is that identifier and keyword parser can overlap, but identifier has a filter on keywords
    */
  case class KeywordParser(value: String) extends ParserBuilderBase[String] with ParserWrapper[String] {
    override def getParser(recursive: GetParser): Parser[String] = {
      val parseIdentifier = recursive(identifier)
      (input, state) => {
        parseIdentifier(input, state).mapReady(ready => {
          if (ready.resultOption.contains(value)) {
            ready
          } else {
            var parsed = 0
            val parsedIdentifier = ready.resultOption.getOrElse("")
            while(parsed < parsedIdentifier.length &&  parsed < value.length &&
              parsedIdentifier.charAt(parsed) == value.charAt(parsed)) {
              parsed += 1
            }
            val insertFix = value.drop(parsed)
            val reached = input.safeDrop(parsed)
            val error =
              if (ready.remainder == input)
                new MissingInput(input, value, insertFix, History.missingInputPenalty)
              else
                MissingInput(input, ready.remainder, value, insertFix, History.missingInputPenalty)

            ReadyParseResult(Some(value), reached, History.error(error))
          }
        }, uniform = false)
      }
    }

    override def original: Self[String] = identifier
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
