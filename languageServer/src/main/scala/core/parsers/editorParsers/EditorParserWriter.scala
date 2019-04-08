package core.parsers.editorParsers

import core.parsers.core.{GraphAlgorithms, LeftRecursiveParserWriter}

import scala.language.higherKinds

trait EditorParserWriter extends LeftRecursiveParserWriter {

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

  case class Succeed[Result](value: Result) extends EditorParserBase[Result] with LeafParser[Result] {
    override def apply(input: Input) = newSuccess(value, input)

    override def getDefault(cache: DefaultCache): Option[Result] = Some(value)

    override def getMustConsume(cache: ConsumeCache) = false
  }

  implicit class EditorParserExtensions[Result](parser: EditorParser[Result]) extends ParserExtensions2(parser) {

    def filter[Other >: Result](predicate: Other => Boolean, getMessage: Other => String) = Filter(parser, predicate, getMessage)

    def withDefault[Other >: Result](_default: Other): EditorParser[Other] =
      WithDefault[Other](parser, cache => Some(_default))

    def parseWholeInput(input: Input): ParseResult[Result] = {
      EditorParserWriter.this.parseWholeInput(parser, input)
    }

    override def parseRoot(input: Input): ParseResult[Result] = {
      setDefaults(parser)
      compile(parser)
      parser.parse(input)
    }

    def withRange[Other >: Result](addRange: (Input, Input, Result) => Other): EditorParser[Other] = {
      val withPosition = leftRight(
        PositionParser,
        WithRemainderParser(parser),
        (left: Input, resultRight: Success[Result]) => addRange(left, resultRight.remainder, resultRight.result))
      WithDefault(withPosition, cache => parser.getDefault(cache))
    }
  }

  trait EditorParserBase[Result] extends ParserBase[Result] with EditorParser[Result] {
    var default: Option[Result] = None
  }

  trait EditorParser[+Result] extends Parser2[Result] with HasGetDefault[Result] {
    def default: Option[Result]
    def getDefault(cache: DefaultCache): Option[Result] = getDefault(cache)
  }

  def newFailure[Result](partial: Option[Result], input: Input, message: String): ParseResult[Result]

  object PositionParser extends EditorParserBase[Input] with LeafParser[Input] {

    override def apply(input: Input) = {
      newSuccess(input, input)
    }

    override def getDefault(cache: DefaultCache): Option[Input] = None

    override def getMustConsume(cache: ConsumeCache) = false
  }

  case class WithRemainderParser[Result](original: Self[Result])
    extends EditorParserBase[Success[Result]] with ParserWrapper[Success[Result]] {

    override def apply(input: Input) = {
      val parseResult = original.parse(input)
      parseResult.flatMap[Success[Result]](success => newSuccess(success, success.remainder))
    }

    override def getDefault(cache: DefaultCache): Option[Success[Result]] = None
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

  class EditorLazy[Result](_inner: => EditorParser[Result]) extends Lazy[Result](_inner) with EditorParserBase[Result] {

    override def getDefault(cache: DefaultCache): Option[Result] = cache(original.asInstanceOf[EditorParser[Result]])
  }

  override def lazyParser[Result](inner: => EditorParser[Result]) = new EditorLazy(inner)

  case class WithDefault[Result](original: Self[Result], _getDefault: DefaultCache => Option[Result])
    extends EditorParserBase[Result] with ParserWrapper[Result] {
    override def apply(input: Input): ParseResult[Result] = {
      val result = original.parse(input)
      if (result.successful) {
        return result
      }

      result.biggestFailure match {
        case failure: ParseFailure[Result] =>
          if (failure.partialResult.isEmpty || failure.remainder == input) {
            val _default = default
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
  }

  case class Filter[Other, Result <: Other](original: EditorParser[Result],
                                             predicate: Other => Boolean, getMessage: Other => String)
    extends EditorParserBase[Result] with ParserWrapper[Result] {
    override def apply(input: Input): ParseResult[Result] = {
      val originalResult = original.parse(input)
      originalResult.flatMap(s => {
        if (predicate(s.result))
          newSuccess(s.result, s.remainder)
        else {
          newFailure(default, s.remainder, getMessage(s.result))
        }
      })
    }

    override def getDefault(cache: DefaultCache): Option[Result] =
      original.getDefault(cache).filter(predicate)
  }

  case class Fail(message: String) extends EditorParserBase[Nothing] with LeafParser[Nothing] {
    override def getDefault(cache: DefaultCache) = None

    override def apply(input: Input) = newFailure(None, input, message)

    override def getMustConsume(cache: ConsumeCache) = false
  }

  def setDefaults(root: Self[_]): Unit = {
    val cache = new DefaultCache
    GraphAlgorithms.depthFirst[Parser2[_]](root, parser => parser.children, (first, path) => if (first) {
      val parser = path.head.asInstanceOf[EditorParserBase[Any]]
      parser.default = parser.getDefault(cache)
    }, _ => {})
  }
}
