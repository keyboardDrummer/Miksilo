package miksilo.editorParser.languages.yaml

import miksilo.editorParser.parsers.core.{Processor, TextPointer}
import miksilo.editorParser.parsers.editorParsers.{History, LeftRecursiveCorrectingParserWriter}
import miksilo.editorParser.parsers.strings.{CommonParserWriter, IndentationSensitiveParserWriter, WhitespaceParserWriter}

object YamlParser extends LeftRecursiveCorrectingParserWriter
  with IndentationSensitiveParserWriter with CommonParserWriter with WhitespaceParserWriter {

  trait YamlContext
  object FlowIn extends YamlContext
  object FlowOut extends YamlContext
  object BlockIn extends YamlContext
  object BlockOut extends YamlContext
  object BlockKey extends YamlContext
  object FlowKey extends YamlContext

  override def startState = MyState(0, BlockOut)

  case class MyState(indentation: Int, context: YamlContext) extends HasIndentation {
    def withContext(newContext: YamlContext): MyState = MyState(indentation, newContext)

    override def withIndentation(newIndentation: Int) = MyState(newIndentation, context)
  }

  override type State = MyState

  class IfContext[Result](inners: Map[YamlContext, Parser[Result]]) extends ParserBuilderBase[Result] {

    override def getParser(recursive: GetParser) = {
      val innerParsers = inners.view.mapValues(p => recursive(p)).toMap
      (position, state, fixPointState) => innerParsers(state.context)(position, state, fixPointState)
    }

    override def leftChildren = inners.values.toList

    override def getMustConsume(cache: ConsumeCache) = inners.values.forall(i => cache(i))

    override def children = leftChildren
  }

  class WithContext[Result](update: YamlContext => YamlContext, val original: Parser[Result])
    extends ParserBuilderBase[Result] with ParserWrapper[Result] {

    override def getParser(recursive: GetParser): BuiltParser[Result] = {
      val parseOriginal = recursive(original)

      def apply(position: TextPointer, state: State, fixPointState: FixPointState): ParseResult[Result] = {
        val context: YamlContext = state.context
        val result = parseOriginal(position, state.withContext(update(context)), fixPointState)
        result.updateRemainder((position, state) => (position, state.withContext(context)))
      }

      apply
    }
  }


  lazy val tag: Parser[String] = "!" ~> RegexParser(s"""[^'\r\n !$flowIndicatorChars]+""".r, "tag name") //Should be 	ns-uri-char - “!” - c-flow-indicator

  lazy val hole = Fallback(RegexParser(" *".r, "spaces").withSourceRange((range,_) => ValueHole(range)), "value")
  lazy val parseUntaggedFlowValue: Parser[YamlValue] = parseBraceObject | parseBracketArray | parseNumber | parseFlowStringLiteral | plainScalar
  lazy val parseFlowValue: Parser[YamlValue] = (tag ~ parseUntaggedFlowValue).
    withSourceRange((range, v) => TaggedNode(Some(range), v._1, v._2)) | parseUntaggedFlowValue
  lazy val flowInBlock = new WithContext(_ => FlowOut, parseFlowValue)
  lazy val parseUntaggedBlockValue: Parser[YamlValue] = new Lazy(parseBlockArray | parseBlockMapping | blockScalar | hole, "untagged value")

  lazy val parseBlockValue: Parser[YamlValue] = (tag ~ parseUntaggedBlockValue).
    withSourceRange((range, v) => TaggedNode(Some(range), v._1, v._2)) | parseUntaggedBlockValue

  lazy val parseValue = flowInBlock | parseBlockValue
  lazy val parseYaml = trivias ~> literal("---").option ~> parseValue ~< trivias
  lazy val parser = parseYaml.getWholeInputParser()

  lazy val parseBlockMapping: Parser[YamlValue] = {
    val member = new WithContext(_ =>
      BlockKey,
      //parseUntaggedFlowValue
      parseFlowValue
    ) ~< literalOrKeyword(":") ~ greaterThan(parseValue)
    alignedList(member).withSourceRange((range, values) => {
      YamlObject(Some(range), values.toArray)
    })
  }

  val newLine = parseRegex("""\r?\n""".r, "newline", penaltyOption = Some(History.failPenalty), allowDrop = false)
  val blockScalar: Parser[StringLiteral] = {
    val nbChar = parseRegex("""[^\r\n]+""".r, "non break character")
    val chompingIndicator: Parser[String] = "-" | "+" | literal("")
    val lineSeparator = leftRightSimple(newLine,
      whiteSpace, Processor.ignoreLeft[String, String])

    val lines: Parser[StringLiteral] = {
      val line = greaterThanOrEqualTo(nbChar)
      greaterThan(new WithIndentation(line.someSeparated(lineSeparator, "line").
        withSourceRange((range, lines) => new StringLiteral(Some(range), lines.reduce((a,b) => a + "\n" + b)))))
    }

    WithIndentation(("|" | ">") ~ chompingIndicator ~ lineSeparator) ~> lines
  }

  lazy val objectMember = parseFlowValue ~< ":" ~ parseFlowValue
  lazy val parseBraceObject = {
    val result = (literal("{", 2 * History.missingInputPenalty) ~> objectMember.manySeparated(",", "member") ~< "}").
      withSourceRange((range, value) => YamlObject(Some(range), value.toArray))

    new WithContext({
      case FlowOut => FlowIn
      case BlockKey => FlowKey
      case FlowIn => FlowIn
      case FlowKey => FlowKey
    }, result)
  }

  lazy val parseBracketArray: Parser[YamlValue] = {
    val inner = "[" ~> parseFlowValue.manySeparated(",", "array element").
      withSourceRange((range, elements) => YamlArray(Some(range), elements.toArray)) ~< "]"
    new WithContext(_ => FlowIn, inner)
  }

  lazy val parseBlockArray: Parser[YamlValue] = {
    val element = literalOrKeyword("- ") ~> greaterThan(parseValue)
    alignedList(element).withSourceRange((range, elements) => YamlArray(Some(range), elements.toArray))
  }

  lazy val parseNumber: Parser[YamlValue] =
    wholeNumber.withSourceRange((range, n) => NumberLiteral(Some(range), n))

  lazy val parseFlowStringLiteral: Parser[YamlValue] =
    parseStringLiteralInner.withSourceRange((range, s) => StringLiteral(Some(range), s))
  lazy val parseStringLiteralInner: Parser[String] =
    RegexParser("""'[^']*'""".r, "single quote string literal").map(n => n.drop(1).dropRight(1))

  lazy val plainScalar = new WithContext({
    case FlowIn => FlowIn
    case BlockKey => BlockKey
    case FlowKey => FlowKey
    case _ => FlowOut
  }, plainStyleMultiLineString | plainStyleSingleLineString).
    withSourceRange((range, s) => StringLiteral(Some(range), s))

  val nonBreakChars = """\r\n"""
  val nonSpaceChars = nonBreakChars + " "

  val flowIndicatorChars = """,\[\]{}"""
  val indicatorChars = """-\?:,\[\]\{\}#&*!\|>'"%@`"""

  val plainSafeOutChars = s"""$nonBreakChars#'"""
  val plainSafeInChars = s"""$plainSafeOutChars$flowIndicatorChars"""

  val allowedInFirst = Set('?',':','-')
  val nonPlainFirstChars = (nonSpaceChars + indicatorChars).filter((c: Char) => !allowedInFirst.contains(c))

  //first char shouldn't be a space.
  val doubleColonPlainSafeIn = RegexParser(s"""[^$nonPlainFirstChars]([^$plainSafeInChars:]|:[^$plainSafeInChars ])*""".r, "plain scalar")
  val doubleColonPlainSafeOut = RegexParser(s"""[^$nonPlainFirstChars]([^$plainSafeOutChars:]|:[^$plainSafeOutChars ])*""".r, "plain scalar")

  val nsPlainSafe = new IfContext(Map(
    FlowIn -> doubleColonPlainSafeIn,
    FlowOut -> doubleColonPlainSafeOut,
    BlockKey -> doubleColonPlainSafeOut,
    FlowKey -> doubleColonPlainSafeIn))

  lazy val plainStyleSingleLineString = nsPlainSafe
  lazy val plainStyleMultiLineString = {
    val firstLine = new Sequence[String, Any, String](nsPlainSafe, whiteSpace, Processor.ignoreRight)
    val followingLines = greaterThan(WithIndentation(equal(nsPlainSafe).someSeparated(newLine, "line")))
    new Sequence(firstLine, followingLines, combineSimple((firstLine: String, rest: List[String]) => {
      rest.fold(firstLine)((a, b) => a + " " + b)
    }))
  }
}
