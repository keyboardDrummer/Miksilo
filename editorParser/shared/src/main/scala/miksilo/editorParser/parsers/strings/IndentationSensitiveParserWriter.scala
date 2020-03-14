package miksilo.editorParser.parsers.strings

import miksilo.editorParser.parsers.core.{TextPointer}
import miksilo.editorParser.parsers.editorParsers.History

trait DefaultIndentationSensitiveWriter extends IndentationSensitiveParserWriter {
  type State = MyState

  override def startState = MyState(0)

  case class MyState(indentation: Int) extends HasIndentation {

    override def withIndentation(newIndentation: Int) = MyState(newIndentation)
  }
}

trait IndentationSensitiveParserWriter extends StringParserWriter {
  trait HasIndentation {
    def indentation: Int
    def withIndentation(newIndentation: Int): State
  }
  type State <: HasIndentation

  case class WithIndentation[Result](original: Parser[Result])
    extends ParserBuilderBase[Result] with ParserWrapper[Result]{

    override def getParser(recursive: GetParser): BuiltParser[Result] = {
      val parseOriginal = recursive(original)

      def apply(position: TextPointer, state: State, fixPointState: FixPointState) = {
        val previous = state
        val lineCharacter = position.lineCharacter
        val result: ParseResult[Result] = parseOriginal(position, state.withIndentation(lineCharacter.character), fixPointState)
        result.updateRemainder((remainder, _) => {
          (remainder, previous)
        })
      }

      apply
    }
  }

  def alignedList[Element](element: Parser[Element]): Parser[List[Element]] = {
    aligned(element, List.empty, (a: Element, b: List[Element]) => a :: b)
  }

  def aligned[Element, Sum](firstLine: Parser[Element], zero: Sum, reduce: (Element, Sum) => Sum): Parser[Sum] = {
    val remainingLines = equal(firstLine).many(zero, reduce)
    WithIndentation(leftRight(firstLine, remainingLines, combineFold(zero, reduce)))
  }

  def equal[Result](inner: Parser[Result]) = CheckIndentation(delta => delta == 0, "equal to", inner)
  def greaterThan[Result](inner: Parser[Result]) = CheckIndentation(delta => delta > 0, "greater than", inner)

  case class IndentationError(from: TextPointer, expectedIndentation: Int, property: String) extends NextCharError {
    override def penalty = History.indentationErrorPenalty

    override def message = {
      val lineCharacter = from.lineCharacter
      s"indentation ${lineCharacter.character} of character '${from.head}' must be $property $expectedIndentation"
    }
  }

  case class CheckIndentation[Result](deltaPredicate: Int => Boolean, property: String, original: Parser[Result])
    extends ParserBuilderBase[Result] with ParserWrapper[Result] {

    override def getParser(recursive: GetParser) = {
      val parseOriginal = recursive(original)

      def apply(position: TextPointer, state: State, fixPointState: FixPointState) = {
        val lineCharacter = position.lineCharacter
        val delta = lineCharacter.character - state.indentation
        if (position.atEnd() || deltaPredicate(delta)) {
          parseOriginal(position, state, fixPointState)
        } else {
          newFailure(None, position, state, History.error(IndentationError(position, lineCharacter.character, property)))
        }
      }
      apply
    }

    override def leftChildren = List(original)

    override def getMustConsume(cache: ConsumeCache) = cache(original)
  }
}
