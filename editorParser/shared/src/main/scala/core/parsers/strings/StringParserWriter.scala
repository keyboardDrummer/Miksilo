package core.parsers.strings

import core.parsers.core.ParseText
import core.parsers.editorParsers.{History, ParseError, ReadyParseResult, SREmpty, SourceRange}
import core.parsers.sequences.SequenceParserWriter

import scala.language.implicitConversions
import scala.util.matching.Regex

trait StringParserWriter extends SequenceParserWriter {
  type Elem = Char
  type Input <: StringReaderLike

  abstract class StringReaderBase(val offsetNode: OffsetNode)
    extends StringReaderLike {

    override def end(array: ParseText) = drop(array.length() - offset)

    override def printRange(text: ParseText, end: Input) = text.subSequence(offset, end.offset).toString

    override def atEnd(array: ParseText): Boolean = offset == array.length

    override def head(array: ParseText): Char = array.charAt(offset)

    override def tail(array: ParseText): Input = drop(1)

    override def hashCode(): Int = offset

    override def equals(obj: Any): Boolean = obj match {
      case other: StringReaderBase => offset == other.offset
      case _ => false
    }

    def print(text: ParseText): String = {
      val position = text.getPosition(offset)
      s"(${position.line}, ${position.character})" +
        text.subSequence(Math.max(0, offset - 10), offset) + " | " + text.subSequence(offset, Math.min(text.length, offset + 10))
    }

    override def toString: String = {
      offset.toString
    }
  }

  trait StringReaderLike extends SequenceInput[Char] {

    def drop(amount: Int): Input
    def remaining(array: ParseText) = array.length() - offset
  }

  val identifierRegex = """[_a-zA-Z][_a-zA-Z0-9]*""".r
  lazy val parseIdentifier = parseRegex(identifierRegex, "identifier")

  implicit def literalToExtensions(value: String): SequenceParserExtensions[String] =
    literalOrKeyword(value)

  implicit def stringToLiteralOrKeyword(value: String): Parser[String] = {
    literalOrKeyword(value)
  }

  def literalOrKeyword(value: String, allowDrop: Boolean = true): Parser[String] = {
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

    override def getParser(text: ParseText, recursive: GetParser): BuiltParser[String] = {

      lazy val result: BuiltParser[String] = new BuiltParser[String] {
        def apply(input: Input, state: ParseState): ParseResult[String] = {
          var index = 0
          while (index < value.length) {
            val arrayIndex = index + input.offset
            val remainder = input.drop(index)
            val errorHistory = History.error(new MissingInput(text, remainder, value.substring(index), value.substring(index), penalty))
            if (text.length <= arrayIndex) {
              return singleResult(ReadyParseResult(Some(value), remainder, errorHistory))
            } else if (text.charAt(arrayIndex) != value.charAt(index)) {
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
    override def getParser(text: ParseText, recursive: GetParser): BuiltParser[String] = {
      val identifierParser = recursive(parseIdentifier)
      new BuiltParser[String] {
        override def apply(input: Input, state: ParseState) = {
          identifierParser(input, state).mapReady(ready => {
            if (ready.resultOption.contains(value)) {
              ready
            } else {
              val insertError = new MissingInput(text, input, value, value + " ")
              ReadyParseResult(Some(value), input, History.error(insertError))
            }
          }, uniform = false)
        }
      }
    }

    override def original: Parser[String] = parseIdentifier
  }

  trait NextCharError extends ParseError[Input] {
    def text: ParseText
    def to: Input = if (this.from.atEnd(text)) this.from else this.from.drop(1)
    def range(text: ParseText) = SourceRange(text.getPosition(from.offset), text.getPosition(to.offset))
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

    override def getParser(text: ParseText, recursive: GetParser): BuiltParser[String] = {

      lazy val result: BuiltParser[String] = new BuiltParser[String] {

        def apply(input: Input, state: ParseState): ParseResult[String] = {
          regex.findPrefixMatchOf(new SubSequence(text, input.offset)) match {
            case Some(matched) =>
              val value = text.subSequence(input.offset, input.offset + matched.end).toString
              val remainder = input.drop(matched.end)
              singleResult(ReadyParseResult(Some(value), remainder, History.success(input, remainder, value, score)))
            case None =>
              penaltyOption.fold[ParseResult[String]](SREmpty.empty)(penalty => {
                val history = History.error(new MissingInput(text, input, s"<$regexName>", defaultValue.getOrElse(""), penalty))
                singleResult(ReadyParseResult(defaultValue, input, history))
              })

          }
        }
      }

      result
    }

    override def getMustConsume(cache: ConsumeCache) = regex.findFirstIn("").isEmpty
  }

  implicit class StringParserExtensions[Result](parser: Parser[Result]) {

    def withSourceRange[Other](addRange: (SourceRange, Result) => Other): Parser[Other] = {
      parser.withRange((text, l,r,v) => addRange(SourceRange(text.getPosition(l.offset), text.getPosition(r.offset)), v))
    }
  }
}
