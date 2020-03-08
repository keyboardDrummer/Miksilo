package languages.yaml

import core.document.Empty
import core.parsers.core.{InputGen, ParseText, Processor}
import core.parsers.editorParsers.{AbsoluteTextPointer, History, LeftRecursiveCorrectingParserWriter, OffsetPointerRange}
import core.parsers.strings.{CommonParserWriter, IndentationSensitiveParserWriter, WhitespaceParserWriter}
import core.responsiveDocument.ResponsiveDocument

trait YamlValue {
  def toDocument: ResponsiveDocument

  override def toString: String = toDocument.renderString()
}

case class YamlObject(range: OffsetPointerRange, members: Array[(YamlValue, YamlValue)]) extends YamlValue {
  override def toDocument: ResponsiveDocument = {
    members.
      map(member => member._1.toDocument ~ ":" ~~ member._2.toDocument).
      reduce((t,b) => t % b)
  }
}

case class YamlArray(range: OffsetPointerRange, elements: Array[YamlValue]) extends YamlValue {
  override def toDocument: ResponsiveDocument = {
    elements.
      map(member => ResponsiveDocument.text("- ") ~~ member.toDocument).
      fold[ResponsiveDocument](Empty)((t: ResponsiveDocument, b: ResponsiveDocument) => t % b)
  }
}

case class NumberLiteral(range: OffsetPointerRange, value: Int) extends YamlValue {
  override def toDocument: ResponsiveDocument = ResponsiveDocument.text(value.toString)
}

case class StringLiteral(range: OffsetPointerRange, value: String) extends YamlValue {
  override def toDocument: ResponsiveDocument = ResponsiveDocument.text(value.toString)
}

case class ValueHole(range: OffsetPointerRange) extends YamlValue {
  override def toDocument: ResponsiveDocument = "hole"
}

case class TaggedNode(range: OffsetPointerRange, tag: String, node: YamlValue) extends YamlValue {
  override def toDocument: ResponsiveDocument = ResponsiveDocument.text("!") ~ tag ~~ node.toDocument
}

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
      (input, state) => innerParsers(input.state.context)(input, state)
    }

    override def leftChildren = inners.values.toList

    override def getMustConsume(cache: ConsumeCache) = inners.values.forall(i => cache(i))

    override def children = leftChildren
  }

  class WithContext[Result](update: YamlContext => YamlContext, val original: Parser[Result])
    extends ParserBuilderBase[Result] with ParserWrapper[Result] {

    override def getParser(recursive: GetParser): BuiltParser[Result] = {
      val parseOriginal = recursive(original)

      def apply(input: Input, state: FixPointState): ParseResult[Result] = {
        val context: YamlContext = input.state.context
        val result = parseOriginal(InputGen(input.position, input.state.withContext(update(context))), state)
        result.updateRemainder(r => InputGen(r.position, r.state.withContext(context)))
      }

      apply
    }
  }

  lazy val tag: Parser[String] = "!" ~> RegexParser(s"""[^'\n !$flowIndicatorChars]+""".r, "tag name") //Should be 	ns-uri-char - “!” - c-flow-indicator

  lazy val hole = Fallback(RegexParser(" *".r, "spaces").withSourceRange((range,_) => ValueHole(range)), "value")
  lazy val parseUntaggedFlowValue: Parser[YamlValue] = parseBraceObject | parseBracketArray | parseStringLiteral
  lazy val parseFlowValue = (tag ~ parseUntaggedFlowValue).
    withSourceRange((range, v) => TaggedNode(range, v._1, v._2)) | parseUntaggedFlowValue
  lazy val parseUntaggedValue = new Lazy(parseBracketArray | parseArray | parseNumber | parseStringLiteral |
    parseBlockMapping | hole, "untagged value")

  lazy val parseValue: Parser[YamlValue] = (tag ~ parseUntaggedValue).
    withSourceRange((range, v) => TaggedNode(range, v._1, v._2)) | parseUntaggedValue

  lazy val parseYaml = trivias ~> parseValue ~< trivias
  lazy val parser = parseYaml.getWholeInputParser()
  def getCachingParser(text: ParseText) = AbsoluteTextPointer.getCachingParser(text, parser)

  lazy val parseBlockMapping: Parser[YamlValue] = {
    val member = new WithContext(_ =>
      BlockKey, parseFlowValue) ~< literalOrKeyword(":") ~ greaterThan(parseValue)
    alignedList(member).withSourceRange((range, values) => {
      YamlObject(range, values.toArray)
    })
  }

  lazy val objectMember = parseFlowValue ~< ":" ~ parseFlowValue
  lazy val parseBraceObject = (literal("{", 2 * History.missingInputPenalty) ~> objectMember.manySeparated(",", "member") ~< "}").
    withSourceRange((range, value) => YamlObject(range, value.toArray))

  lazy val parseBracketArray: Parser[YamlValue] = {
    val inner = "[" ~> parseFlowValue.manySeparated(",", "array element").
      withSourceRange((range, elements) => YamlArray(range, elements.toArray)) ~< "]"
    new WithContext(_ => FlowIn, inner)
  }

  lazy val parseArray: Parser[YamlValue] = {
    val element = literalOrKeyword("- ") ~> greaterThan(parseValue)
    alignedList(element).withSourceRange((range, elements) => YamlArray(range, elements.toArray))
  }

  lazy val parseNumber: Parser[YamlValue] =
    wholeNumber.withSourceRange((range, n) => NumberLiteral(range, Integer.parseInt(n)))

  lazy val parseStringLiteral: Parser[YamlValue] =
    parseStringLiteralInner.withSourceRange((range, s) => StringLiteral(range, s))
  lazy val parseStringLiteralInner: Parser[String] =
    RegexParser("""'[^']*'""".r, "single quote string literal").map(n => n.drop(1).dropRight(1)) | plainScalar

  lazy val plainScalar = new WithContext({
    case FlowIn => FlowIn
    case BlockKey => BlockKey
    case FlowKey => FlowKey
    case _ => FlowOut
  }, plainStyleMultiLineString | plainStyleSingleLineString)

  val nbChars = """\n"""
  val nsChars = nbChars + " "

  def flowIndicatorChars = """,\[\]{}"""

  val plainSafeOutChars = s"""$nbChars#'"""
  val plainSafeInChars = s"""$plainSafeOutChars$flowIndicatorChars"""

  //first char shouldn't be a space.
  val doubleColonPlainSafeIn = RegexParser(s"""([^$plainSafeInChars: ]|:[^$plainSafeInChars ])([^$plainSafeInChars:]|:[^$plainSafeInChars ])*""".r, "plain scalar")
  val doubleColonPlainSafeOut = RegexParser(s"""([^$plainSafeInChars: ]|:[^$plainSafeInChars ])([^$plainSafeOutChars:]|:[^$plainSafeOutChars ])*""".r, "plain scalar")

  val nsPlainSafe = new IfContext(Map(
    FlowIn -> doubleColonPlainSafeIn,
    FlowOut -> doubleColonPlainSafeOut,
    BlockKey -> doubleColonPlainSafeOut,
    FlowKey -> doubleColonPlainSafeIn))

  lazy val plainStyleSingleLineString = nsPlainSafe
  lazy val plainStyleMultiLineString = {
    val firstLine = new Sequence[String, Any, String](nsPlainSafe, whiteSpace, Processor.ignoreRight)
    val followingLines = greaterThan(WithIndentation(equal(nsPlainSafe).someSeparated("\n", "line")))
    new Sequence(firstLine, followingLines, combineSimple((firstLine: String, rest: List[String]) => {
      rest.fold(firstLine)((a, b) => a + " " + b)
    }))
  }
}
