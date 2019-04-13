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
    def addDefault[Other >: Result](value: Other, force: Boolean = false): ParseResult[Other]
  }

  override def succeed[Result](result: Result): EditorParser[Result] = Succeed(result)

  override def fail[Result](message: String) = Fail(message)

  def parseWholeInput[Result](parser: EditorParser[Result], input: Input): ParseResult[Result]

  case class Succeed[Result](value: Result) extends EditorParserBase[Result] with LeafParser[Result] {

    override def getParser(recursive: GetParse): Parse[Result] = {
      input => newSuccess(value, input)
    }

    override def getDefault(cache: DefaultCache): Option[Result] = Some(value)

    override def getMustConsume(cache: ConsumeCache) = false
  }

  implicit class EditorParserExtensions[Result](parser: EditorParser[Result]) extends ParserExtensions(parser) {

    def filter[Other >: Result](predicate: Other => Boolean, getMessage: Other => String) = Filter(parser, predicate, getMessage)

    def withDefault[Other >: Result](_default: Other): EditorParser[Other] =
      WithDefault[Other](parser, cache => Some(_default))

    def parseWholeInput(input: Input): ParseResult[Result] = {
      EditorParserWriter.this.parseWholeInput(parser, input)
    }

    override def parseRoot(input: Input): ParseResult[Result] = {
      setDefaults(parser)
      val analysis = compile(parser)
      analysis.getParse(parser)(input)
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

  trait EditorParser[+Result] extends LRParser[Result] with HasGetDefault[Result] {
    def default: Option[Result]
    def getDefault(cache: DefaultCache): Option[Result] = getDefault(cache)
  }

  class MapParser[Result, NewResult](val original: EditorParser[Result], f: Result => NewResult)
    extends EditorParserBase[NewResult] with ParserWrapper[NewResult] {

    override def getParser(recursive: GetParse): Parse[NewResult] = {
      val parseOriginal = recursive(original)
      input => parseOriginal(input).map(f)
    }

    override def getDefault(cache: DefaultCache): Option[NewResult] = cache(original).map(f)

    override def getMustConsume(cache: ConsumeCache) = cache(original)
  }

  def newFailure[Result](partial: Option[Result], input: Input, message: String): ParseResult[Result]

  object PositionParser extends EditorParserBase[Input] with LeafParser[Input] {

    override def getParser(recursive: GetParse): Parse[Input] = {
      input => newSuccess(input, input)
    }

    override def getDefault(cache: DefaultCache): Option[Input] = None

    override def getMustConsume(cache: ConsumeCache) = false
  }

  case class WithRemainderParser[Result](original: Self[Result])
    extends EditorParserBase[Success[Result]] with ParserWrapper[Success[Result]] {


    override def getParser(recursive: GetParse): Parse[Success[Result]] = {
      val parseOriginal = recursive(original)
      input => parseOriginal(input).flatMap(success => newSuccess(success, success.remainder))
    }

    override def getDefault(cache: DefaultCache): Option[Success[Result]] = None
  }

  case class ParseFailure[+Result](partialResult: Option[Result], remainder: Input, message: String)
    extends OptionFailure[Result] {

    override def map[NewResult](f: Result => NewResult): ParseFailure[NewResult] =
      ParseFailure(partialResult.map(r => f(r)), remainder, message)

    override def offset: Int = remainder.offset

    override def toString: String = message

    def addDefault[Other >: Result](value: Other, force: Boolean): ParseFailure[Other] = partialResult match {
      case Some(_) if !force => this
      case _ => ParseFailure(Some(value), remainder, message)
    }

    override def updateRemainder(f: Input => Input) = ParseFailure(partialResult, f(remainder), message)
  }

  trait OptionFailure[+Result] {
    def offset: Int
    def partialResult: Option[Result]
    def map[NewResult](f: Result => NewResult): OptionFailure[NewResult]
    def updateRemainder(f: Input => Input): OptionFailure[Result]

    def getBiggest[Other >: Result](other: OptionFailure[Other]): OptionFailure[Other] = {
      if (offset > other.offset) this else other
    }
  }

  class EditorLazy[Result](_inner: => EditorParser[Result]) extends Lazy[Result](_inner) with EditorParserBase[Result] {

    override def getDefault(cache: DefaultCache): Option[Result] = cache(original.asInstanceOf[EditorParser[Result]])
  }

  override def lazyParser[Result](inner: => EditorParser[Result]) = new EditorLazy(inner)

  case class WithDefault[Result](original: Self[Result], _getDefault: DefaultCache => Option[Result])
    extends EditorParserBase[Result] with ParserWrapper[Result] {

    override def getParser(recursive: GetParse): Parse[Result] = {
      val parseOriginal = recursive(original)

      def apply(input: Input): ParseResult[Result] = {
        val result = parseOriginal(input)
        if (result.successful) {
          return result
        }

        result.biggestFailure match {
          case failure: ParseFailure[Result] =>
            if (failure.partialResult.isEmpty || failure.remainder == input) {
              val _default = default
              if (_default.nonEmpty) {
                return result.addDefault(_default.get, force = true)
              }
            }
          case _ =>
        }
        result
      }

      apply
    }

    override def getDefault(cache: DefaultCache): Option[Result] =
      _getDefault(cache)
  }

  case class Filter[Other, Result <: Other](original: EditorParser[Result],
                                             predicate: Other => Boolean, getMessage: Other => String)
    extends EditorParserBase[Result] with ParserWrapper[Result] {


    override def getParser(recursive: GetParse): Parse[Result] = {
      val parseOriginal = recursive(original)
      input => {
        val originalResult = parseOriginal(input)
        originalResult.flatMap(s => {
          if (predicate(s.result))
            newSuccess(s.result, s.remainder)
          else {
            newFailure(default, s.remainder, getMessage(s.result))
          }
        })
      }
    }

    override def getDefault(cache: DefaultCache): Option[Result] =
      original.getDefault(cache).filter(predicate)
  }

  case class Fail(message: String) extends EditorParserBase[Nothing] with LeafParser[Nothing] {
    override def getDefault(cache: DefaultCache) = None


    override def getParser(recursive: GetParse): Parse[Nothing] = {
      input => newFailure(None, input, message)
    }

    override def getMustConsume(cache: ConsumeCache) = false
  }

  def setDefaults(root: Self[_]): Unit = {
    val cache = new DefaultCache
    GraphAlgorithms.depthFirst[LRParser[_]](root, parser => parser.children, (first, path) => if (first) {
      val parser = path.head.asInstanceOf[EditorParserBase[Any]]
      parser.default = parser.getDefault(cache)
    }, _ => {})
  }
}
