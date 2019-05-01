package core.parsers.editorParsers

import core.language.node.SourceRange
import core.parsers.core.{GraphAlgorithms, OptimizingParserWriter, ParseInput}
import langserver.types.Position

import scala.language.higherKinds

trait CorrectingInput extends ParseInput {
  def offsetScore: Int
  def position: Position
}

trait EditorParserWriter extends OptimizingParserWriter {

  type Input <: CorrectingInput

  type Self[+Result] = EditorParser[Result]

  override def succeed[Result](result: Result): EditorParser[Result] = Succeed(result)

  case class ParseWholeResult[Result](resultOption: Option[Result], errors: List[ParseErrorLike]) {
    def successful = errors.isEmpty
    def get: Result = resultOption.get
  }

  def parseWholeInput[Result](parser: EditorParser[Result], input: Input): ParseWholeResult[Result]

  case class Succeed[Result](value: Result) extends EditorParserBase[Result] with LeafParser[Result] {

    override def getParser(recursive: GetParse): Parse[Result] = {
      (input: Input, state) => newSuccess(value, input)
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

      new Parse[NewResult] {
        override def apply(input: Input, state: ParseState): ParseResult[NewResult] = parseOriginal(input, state).map(f)
      }
    }

    override def getDefault(cache: DefaultCache): Option[NewResult] = cache(original).map(f)

    override def getMustConsume(cache: ConsumeCache) = cache(original)
  }

  def newFailure[Result](partial: Option[Result], input: Input, errors: Errors): ParseResult[Result]

  object PositionParser extends EditorParserBase[Input] with LeafParser[Input] {

    override def getParser(recursive: GetParse): Parse[Input] = {
      (input, state) => newSuccess(input, input)
    }

    override def getDefault(cache: DefaultCache): Option[Input] = None

    override def getMustConsume(cache: ConsumeCache) = false
  }

  case class Errors(errors: List[ParseErrorLike]) {

    def this() = this(List.empty)
    def this(error: ParseErrorLike) = this(List(error))

    def errorSize = errors.map(e => e.penalty).sum

    def ++(others: Errors): Errors = errors.foldRight(others)((e, r) => r.add(e))
    def isEmpty: Boolean = errors.isEmpty
    def nonEmpty: Boolean = errors.nonEmpty

    def add(error: ParseErrorLike): Errors = {
      errors match {
        case Nil => new Errors(error)
        case head :: tail =>
          val merged = head.append(error)
          Errors(merged.fold(error :: errors)(m => m :: tail))
      }
    }
  }

  trait ParseErrorLike {
    def penalty: Double
    def append(other: ParseErrorLike): Option[ParseErrorLike] = None
    def message: String
    def range: SourceRange

    override def toString = message
  }

  case class ParseError(location: Input, message: String, penalty: Double = 1) extends ParseErrorLike {
    override def append(other: ParseErrorLike): Option[ParseErrorLike] = None

    override def range = {
      val position = location.position
      SourceRange(position, Position(position.line, position.character + 1))
    }
  }

  class EditorLazy[Result](_inner: => EditorParser[Result]) extends Lazy[Result](_inner) with EditorParserBase[Result] {

    override def getDefault(cache: DefaultCache): Option[Result] = cache(original.asInstanceOf[EditorParser[Result]])
  }

  override def lazyParser[Result](inner: => EditorParser[Result]) = new EditorLazy(inner)


  case class Fail[Result](value: Option[Result], message: String) extends EditorParserBase[Result] with LeafParser[Result] {
    override def getDefault(cache: DefaultCache) = None

    override def getParser(recursive: GetParse): Parse[Result] = {
      (input, _) => newFailure(value, input, new Errors(ParseError(input, message, 0.1)))
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
