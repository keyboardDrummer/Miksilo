package core.parsers.editorParsers

import core.language.node.SourceRange
import core.parsers.core.{GraphAlgorithms, OptimizingParserWriter, ParseInput}
import langserver.types.Position

import scala.language.higherKinds


trait EditorParserWriter extends OptimizingParserWriter {

  case class MissingInput(location: Input, message: String, penalty: Double = 1) extends ParseError {
    override def append(other: ParseError): Option[ParseError] = None

    override def range = {
      val position = location.position
      SourceRange(position, Position(position.line, position.character + 1))
    }

    override def from = location

    override def to = from
  }

  trait CorrectingInput extends ParseInput {
    def offsetScore: Int
    def position: Position
  }

  type Input <: CorrectingInput

  type Self[+Result] = EditorParser[Result]

  override def succeed[Result](result: Result): EditorParser[Result] = Succeed(result)

  case class ParseWholeResult[Result](resultOption: Option[Result], errors: List[ParseError]) {
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

  def newFailure[Result](partial: Option[Result], input: Input, errors: History): ParseResult[Result]

  object PositionParser extends EditorParserBase[Input] with LeafParser[Input] {

    override def getParser(recursive: GetParse): Parse[Input] = {
      (input, _) => newSuccess(input, input)
    }

    override def getDefault(cache: DefaultCache): Option[Input] = None

    override def getMustConsume(cache: ConsumeCache) = false
  }

  case class History(score: Double, errors: List[ParseError]) {
    def this() = this(0, List.empty)
    def this(error: ParseError) = this(error.score, List(error))

    def ++(right: History): History = {
      if (errors.isEmpty)
        return History(score + right.score, right.errors)

      val (withoutLast, last :: Nil) = errors.splitAt(errors.length - 1)
      val newLeft = History(score - last.score, withoutLast)
      val newRight = right.addError(last)
      History(newLeft.score + newRight.score, newLeft.errors ++ newRight.errors)
    }

    def addSuccess(end: Input): History = {
      History(score + 1, errors)
    }

    def addError(newHead: ParseError): History = {
      errors match {
        case Nil => History(score + newHead.score, List(newHead))
        case head :: tail =>
          head.append(newHead) match {
            case None => History(score + newHead.score, newHead :: errors)
            case Some(merged) => History(score - head.score + merged.score, merged :: tail)
          }
      }
    }
  }

  trait ParseError {
    def penalty: Double
    def score: Double = -penalty * 2
    def append(other: ParseError): Option[ParseError] = None
    def message: String
    def from: Input
    def to: Input
    def range: SourceRange

    override def toString = message
  }

  class EditorLazy[Result](_inner: => EditorParser[Result]) extends Lazy[Result](_inner) with EditorParserBase[Result] {

    override def getDefault(cache: DefaultCache): Option[Result] = cache(original.asInstanceOf[EditorParser[Result]])
  }

  override def lazyParser[Result](inner: => EditorParser[Result]) = new EditorLazy(inner)

  case class Fail[Result](value: Option[Result], message: String) extends EditorParserBase[Result] with LeafParser[Result] {
    override def getDefault(cache: DefaultCache) = None

    override def getParser(recursive: GetParse): Parse[Result] = {
      (input, _) => newFailure(value, input, new History(MissingInput(input, message, 0.1)))
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
