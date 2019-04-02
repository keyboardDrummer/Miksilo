package core.parsers.editorParsers

import core.parsers.core.ParserWriter

import scala.language.higherKinds

trait EditorParserWriter extends ParserWriter {

  type ExtraState = DefaultCache
  type Self[+Result] = EditorParser[Result]
  type ParseResult[+Result] <: EditorResult[Result]

  trait EditorResult[+Result] extends ParseResultLike[Result] {
    def biggestFailure: OptionFailure[Result]
    def resultOption: Option[Result]
    def updateRemainder(f: Input => Input): ParseResult[Result]
  }

  override def succeed[Result](result: Result): EditorParser[Result] = Succeed(result)

  override def fail[Result](message: String) = Fail(message)

  def parseWholeInput[Result](parser: EditorParser[Result], input: Input): ParseResult[Result]

  def newParseState(parser: EditorParser[_]): ParseStateLike

  case class Succeed[+Result](value: Result) extends EditorParser[Result] {
    override def parseInternal(input: Input, cache: ParseStateLike): ParseResult[Result] = newSuccess(value, input)

    override def getDefault(cache: DefaultCache): Option[Result] = Some(value)

    override def children = List.empty
  }

  implicit class EditorParserExtensions[+Result](parser: EditorParser[Result]) extends ParserExtensions(parser) {

    def filter[Other >: Result](predicate: Other => Boolean, getMessage: Other => String) = Filter(parser, predicate, getMessage)

    def withDefault[Other >: Result](_default: Other): EditorParser[Other] =
      WithDefault[Other](parser, cache => Some(_default))

    def parseWholeInput(input: Input): ParseResult[Result] = {
      EditorParserWriter.this.parseWholeInput(parser, input)
    }

    def parse(input: Input): ParseResult[Result] = {
      val state = newParseState(parser)
      state.getParse(parser)(input)
    }

    def withRange[Other >: Result](addRange: (Input, Input, Result) => Other): EditorParser[Other] = {
      val withPosition = leftRight(
        new PositionParser(),
        new WithRemainderParser(parser),
        (left: Input, resultRight: Success[Result]) => addRange(left, resultRight.remainder, resultRight.result))
      WithDefault(withPosition, cache => parser.getDefault(cache))
    }
  }

  trait EditorParser[+Result] extends Parser[Result] with HasGetDefault[Result] {
    final def getDefault(state: ParseStateLike): Option[Result] = getDefault(state.extraState)
  }

  def newFailure[Result](partial: Option[Result], input: Input, message: String): ParseResult[Result]

  class PositionParser extends EditorParser[Input] {

    override def parseInternal(input: Input, state: ParseStateLike): ParseResult[Input] = {
      newSuccess(input, input)
    }

    override def getDefault(cache: DefaultCache): Option[Input] = None

    override def children = List.empty
  }

  class WithRemainderParser[Result](original: Self[Result])
    extends EditorParser[Success[Result]] {

    override def parseInternal(input: Input, parseState: ParseStateLike): ParseResult[Success[Result]] = {
      val parseResult = parseState.getParse(original)(input)
      parseResult.flatMap[Success[Result]](success => newSuccess(success, success.remainder))
    }

    override def getDefault(cache: DefaultCache): Option[Success[Result]] = None

    override def children = List(original)
  }

  case class ParseFailure[+Result](partialResult: Option[Result], remainder: Input, message: String)
    extends OptionFailure[Result] {

    override def map[NewResult](f: Result => NewResult): ParseFailure[NewResult] =
      ParseFailure(partialResult.map(r => f(r)), remainder, message)

    override def offset: Int = remainder.offset

    override def toString: String = message

    def addDefault[Other >: Result](value: Other): ParseFailure[Other] = partialResult match {
      case _: Some[Result] => this
      case None => ParseFailure(Some(value), remainder, message)
    }

    override def mapRemainder(f: Input => Input) = ParseFailure(partialResult, f(remainder), message)
  }

  trait OptionFailure[+Result] {
    def offset: Int
    def partialResult: Option[Result]
    def map[NewResult](f: Result => NewResult): OptionFailure[NewResult]
    def mapRemainder(f: Input => Input): OptionFailure[Result]

    def getBiggest[Other >: Result](other: OptionFailure[Other]): OptionFailure[Other] = {
      if (offset > other.offset) this else other
    }
  }

  object NoFailure extends OptionFailure[Nothing] {
    override def offset: Int = -1

    override def map[NewResult](f: Nothing => NewResult): OptionFailure[NewResult] = this

    override def partialResult: Option[Nothing] = None

    override def mapRemainder(f: Input => Input) = this
  }

  class EditorLazy[+Result](_inner: => EditorParser[Result]) extends Lazy[Result](_inner) with EditorParser[Result] {

    override def getDefault(cache: DefaultCache): Option[Result] = cache(inner.asInstanceOf[EditorParser[Result]])
  }

  override def lazyParser[Result](inner: => EditorParser[Result]) = new EditorLazy(inner)

  case class WithDefault[+Result](original: Self[Result], _getDefault: DefaultCache => Option[Result])
    extends EditorParser[Result] {
    override def parseInternal(input: Input, state: ParseStateLike): ParseResult[Result] = {
      val result = state.getParse(original)(input)
      if (result.successful) {
        return result
      }

      result.biggestFailure match {
        case failure: ParseFailure[Result] =>
          if (failure.partialResult.isEmpty || failure.remainder == input) {
            val _default = getDefault(state.extraState)
            if (_default.nonEmpty) {
              return newFailure(_default, failure.remainder, failure.message)
          }
        }
        case _ =>
      }

      result
    }

    override def getDefault(cache: DefaultCache): Option[Result] =
      _getDefault(cache)

    override def children = List(original)
  }

  case class Filter[Other, +Result <: Other](original: EditorParser[Result],
                                             predicate: Other => Boolean, getMessage: Other => String)
    extends EditorParser[Result] {
    override def parseInternal(input: Input, state: ParseStateLike): ParseResult[Result] = {
      val originalResult = original.parseInternal(input, state)
      originalResult.flatMap(s => {
        if (predicate(s.result))
          newSuccess(s.result, s.remainder)
        else {
          newFailure(this.getDefault(state), s.remainder, getMessage(s.result))
        }
      })
    }

    override def getDefault(cache: DefaultCache): Option[Result] =
      original.getDefault(cache).filter(predicate)

    override def children = List(original)
  }

  case class Fail(message: String) extends EditorParser[Nothing] {
    override def getDefault(cache: DefaultCache) = None

    override def parseInternal(input: Input, state: ParseStateLike) = newFailure(None, input, message)

    override def children = List.empty
  }
}
