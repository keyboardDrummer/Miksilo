package core.parsers.strings

import core.parsers.core.TextPointer
import core.parsers.editorParsers._
import core.parsers.sequences.SequenceParserWriter

import scala.language.implicitConversions
import scala.util.matching.Regex

trait StringParserWriter extends SequenceParserWriter with LeftRecursiveCorrectingParserWriter {
  type Elem = Char

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
    Literal(value, penalty) // if (allowDrop) DropParser(Literal(value, penalty)) else Literal(value, penalty)

  case class Literal(value: String, penalty: Double = History.missingInputPenalty) extends ParserBuilderBase[String] with LeafParser[String] {

    override def getParser(recursive: GetParser): BuiltParser[String] = {

      lazy val result: BuiltParser[String] = new BuiltParser[String] {
        def apply(position: TextPointer, state: State, fixPointState: FixPointState): ParseResult[String] = {
          var index = 0
          while (index < value.length) {
            val arrayIndex = index + position.offset
            val remainder = position.drop(index)
            val errorHistory = History.error(new MissingInput(remainder, value.substring(index), value.substring(index), penalty))
            if (position.length <= arrayIndex) {
              return singleResult(ReadyParseResult(Some(value), remainder, state, errorHistory))
            } else if (position.charAt(arrayIndex) != value.charAt(index)) {
              return singleResult(ReadyParseResult(Some(value), remainder, state, errorHistory))
            }
            index += 1
          }
          val remainder = position.drop(value.length)
          singleResult(ReadyParseResult(Some(value), remainder, state, History.success(position, remainder, value)))
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
    override def getParser(recursive: GetParser): BuiltParser[String] = {
      val identifierParser = recursive(parseIdentifier)
      new BuiltParser[String] {
        override def apply(position: TextPointer, state: State, fixPointState: FixPointState): ParseResults[State, String] = {
          identifierParser(position, state, fixPointState).mapReady(ready => {
            if (ready.resultOption.contains(value)) {
              ready
            } else {
              val insertError = new MissingInput(position, value, value + " ")
              ReadyParseResult(Some(value), position, state, History.error(insertError))
            }
          }, uniform = false)
        }
      }
    }

    override def original: Parser[String] = parseIdentifier
  }

  trait NextCharError extends ParseError {
    def to: TextPointer = if (this.from.atEnd()) this.from else this.from.drop(1)
  }

  def parseRegex(regex: Regex, regexName: String,
                 // TODO use the regex to generate a default case.
                 defaultValue: Option[String] = None,
                 score: Double = History.successValue,
                 penaltyOption: Option[Double] = Some(History.missingInputPenalty),
                 allowDrop: Boolean = true) = {
    val initial = RegexParser(regex, regexName, defaultValue, score, penaltyOption)
    initial //if (allowDrop) DropParser(initial) else initial
  }

  case class RegexParser(regex: Regex, regexName: String,
                         // TODO use the regex to generate a default case.
                         defaultValue: Option[String] = None,
                         score: Double = History.successValue,
                         penaltyOption: Option[Double] = Some(History.missingInputPenalty))
    extends ParserBuilderBase[String] with LeafParser[String] {

    override def getParser(recursive: GetParser): BuiltParser[String] = {

      lazy val result: BuiltParser[String] = new BuiltParser[String] {

        def apply(position: TextPointer, state: State, fixPointState: FixPointState): ParseResult[String] = {
          regex.findPrefixMatchOf(position.charSequence) match {
            case Some(matched) =>
              val value = position.subSequence(position.offset, position.offset + matched.end).toString
              val remainder = position.drop(matched.end)
              singleResult(ReadyParseResult(Some(value), remainder, state, History.success(position, remainder, value, score)))
            case None =>
              penaltyOption.fold[ParseResult[String]](SREmpty.empty)(penalty => {
                val history = History.error(new MissingInput(position, s"<$regexName>", defaultValue.getOrElse(""), penalty))
                singleResult(ReadyParseResult[State, String](defaultValue, position, state, history))
              })

          }
        }
      }

      result
    }

    override def getMustConsume(cache: ConsumeCache) = regex.findFirstIn("").isEmpty
  }

  implicit class StringParserExtensions[Result](parser: Parser[Result]) {

    def withSourceRange[Other](addRange: (OffsetPointerRange, Result) => Other): Parser[Other] = {
      parser.withRange((l,r,v) => addRange(OffsetPointerRange(l, r), v))
    }
  }
}
