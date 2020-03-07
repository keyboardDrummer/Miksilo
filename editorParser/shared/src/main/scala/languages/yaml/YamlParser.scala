package languages.yaml

import core.document.Empty
import core.parsers.core.{ParseText, Processor}
import core.parsers.editorParsers.{LeftRecursiveCorrectingParserWriter, OffsetNodeRange}
import core.parsers.strings.{CommonParserWriter, IndentationSensitiveParserWriter, WhitespaceParserWriter}
import core.responsiveDocument.ResponsiveDocument

trait YamlValue {
  def toDocument: ResponsiveDocument

  override def toString: String = toDocument.renderString()
}

case class YamlObject(range: OffsetNodeRange, members: Map[YamlValue, YamlValue]) extends YamlValue {
  override def toDocument: ResponsiveDocument = {
    members.
      map(member => member._1.toDocument ~ ":" ~~ member._2.toDocument).
      reduce((t,b) => t % b)
  }
}

case class YamlArray(range: OffsetNodeRange, elements: Seq[YamlValue]) extends YamlValue {
  override def toDocument: ResponsiveDocument = {
    elements.
      map(member => ResponsiveDocument.text("- ") ~~ member.toDocument).
      fold[ResponsiveDocument](Empty)((t: ResponsiveDocument, b: ResponsiveDocument) => t % b)
  }
}

case class NumberLiteral(range: OffsetNodeRange, value: Int) extends YamlValue {
  override def toDocument: ResponsiveDocument = ResponsiveDocument.text(value.toString)
}

case class StringLiteral(range: OffsetNodeRange, value: String) extends YamlValue {
  override def toDocument: ResponsiveDocument = ResponsiveDocument.text(value.toString)
}

case class ValueHole(range: OffsetNodeRange) extends YamlValue {
  override def toDocument: ResponsiveDocument = "hole"
}

case class TaggedNode(range: OffsetNodeRange, tag: String, node: YamlValue) extends YamlValue {
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

  type Input = IndentationReader

  override def startInput(offsetManager: OffsetManager) = new IndentationReader(offsetManager.getOffsetNode(0), BlockOut, 0)

  type CacheKey = (BuiltParser[_], Set[BuiltParser[Any]], Int, YamlContext)

  class IndentationReader(offsetNode: CachingOffsetNode, val context: YamlContext, val indentation: Int)
    extends StringReaderBase(offsetNode) with IndentationReaderLike {

    override def withIndentation(value: Int) = new IndentationReader(offsetNode, context, value)

    def withContext(newState: YamlContext): IndentationReader = new IndentationReader(offsetNode, newState, indentation)

    override def drop(amount: Int) =
      new IndentationReader(offsetNode.drop(amount), context, indentation)

    override def hashCode(): Int = offset ^ indentation ^ context.hashCode()

    override def equals(obj: Any): Boolean = obj match {
      case other: IndentationReader => offset == other.offset && indentation == other.indentation && context == other.context
      case _ => false
    }

    override def createCacheKey(parser: BuiltParser[_], state: Set[BuiltParser[Any]]) = (parser, state, indentation, context)
  }

  class IfContext[Result](inners: Map[YamlContext, Parser[Result]]) extends ParserBuilderBase[Result] {

    override def getParser(text: ParseText, recursive: GetParser) = {
      val innerParsers = inners.view.mapValues(p => recursive(p)).toMap
      (input, state) => innerParsers(input.context)(input, state)
    }

    override def leftChildren = inners.values.toList

    override def getMustConsume(cache: ConsumeCache) = inners.values.forall(i => cache(i))

    override def children = leftChildren
  }

  class WithContext[Result](update: YamlContext => YamlContext, val original: Parser[Result])
    extends ParserBuilderBase[Result] with ParserWrapper[Result] {

    override def getParser(text: ParseText, recursive: GetParser): BuiltParser[Result] = {
      val parseOriginal = recursive(original)

      def apply(input: IndentationReader, state: FixPointState): ParseResult[Result] = {
        val context: YamlContext = input.context
        val result = parseOriginal(input.withContext(update(context)), state)
        result.updateRemainder(r => r.withContext(context))
      }

      apply
    }
  }

  val tag: Parser[String] = "!" ~> RegexParser(s"""[^'\n !$flowIndicatorChars]+""".r, "tag name") //Should be 	ns-uri-char - “!” - c-flow-indicator

  val hole = Fallback(RegexParser(" *".r, "spaces").withSourceRange((range,_) => ValueHole(range)), "value")
  lazy val parseUntaggedFlowValue = parseBracketArray | parseStringLiteral
  lazy val parseFlowValue = (tag ~ parseUntaggedFlowValue).
    withSourceRange((range, v) => TaggedNode(range, v._1, v._2)) | parseUntaggedFlowValue
  lazy val parseUntaggedValue = new Lazy(parseBracketArray | parseArray | parseNumber | parseStringLiteral |
    parseBlockMapping | hole, "untagged value")

  lazy val parseValue: Parser[YamlValue] = (tag ~ parseUntaggedValue).
    withSourceRange((range, v) => TaggedNode(range, v._1, v._2)) | parseUntaggedValue

  lazy val parseYaml = parseValue ~< trivias
  def getParser(text: ParseText = new ParseText()) = parseYaml.getWholeInputParser(text)

  lazy val parseBlockMapping: Parser[YamlValue] = {
    val member = new WithContext(_ =>
      BlockKey, parseFlowValue) ~< literalOrKeyword(":") ~ greaterThan(parseValue)
    alignedList(member).withSourceRange((range, values) => {
      YamlObject(range, values.toMap)
    })
  }

  lazy val parseBracketArray: Parser[YamlValue] = {
    val inner = "[" ~> parseFlowValue.manySeparated(",", "array element").
      withSourceRange((range, elements) => YamlArray(range, elements)) ~< "]"
    new WithContext(_ => FlowIn, inner)
  }

  lazy val parseArray: Parser[YamlValue] = {
    val element = literalOrKeyword("- ") ~> greaterThan(parseValue)
    alignedList(element).withSourceRange((range, elements) => YamlArray(range, elements))
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
