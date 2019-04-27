package core.parsers.editorParsers

import core.parsers.core.{GraphAlgorithms, LeftRecursiveParserWriter}

import scala.language.higherKinds

trait EditorParserWriter extends LeftRecursiveParserWriter {

  type Self[+Result] = EditorParser[Result]

  override def succeed[Result](result: Result): EditorParser[Result] = Succeed(result)

  override def fail[Result](message: String) = Fail(message)

  case class ParseWholeResult[Result](resultOption: Option[Result], errors: List[ParseError]) {
    def successful = errors.isEmpty
    def get: Result = resultOption.get
  }

  def parseWholeInput[Result](parser: EditorParser[Result], input: Input): ParseWholeResult[Result]

  case class Succeed[Result](value: Result) extends EditorParserBase[Result] with LeafParser[Result] {

    override def getParser(recursive: GetParse): Parse[Result] = {
      input: Input => newSuccess(value, input)
    }

    override def getDefault(cache: DefaultCache): Option[Result] = Some(value)

    override def getMustConsume(cache: ConsumeCache) = false
  }

  trait EditorParserBase[Result] extends ParserBase[Result] with EditorParser[Result] {
    var default: Option[Result] = None
  }

  trait EditorParser[+Result] extends LRParser[Result] with HasGetDefault[Result] {
    def default: Option[Result]
    def getDefault(cache: DefaultCache): Option[Result]
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

  final def newFailure[Result](partial: Option[Result], input: Input, message: String): ParseResult[Result] =
    newFailure(partial, input, List(ParseError(input, message)))

  def newFailure[Result](partial: Option[Result], input: Input, errors: List[ParseError]): ParseResult[Result]

  object PositionParser extends EditorParserBase[Input] with LeafParser[Input] {

    override def getParser(recursive: GetParse): Parse[Input] = {
      input => newSuccess(input, input)
    }

    override def getDefault(cache: DefaultCache): Option[Input] = None

    override def getMustConsume(cache: ConsumeCache) = false
  }

  case class ParseError(location: Input, message: String, edits: Int = 1)

  class EditorLazy[Result](_inner: => EditorParser[Result]) extends Lazy[Result](_inner) with EditorParserBase[Result] {

    override def getDefault(cache: DefaultCache): Option[Result] = cache(original.asInstanceOf[EditorParser[Result]])
  }

  override def lazyParser[Result](inner: => EditorParser[Result]) = new EditorLazy(inner)


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
