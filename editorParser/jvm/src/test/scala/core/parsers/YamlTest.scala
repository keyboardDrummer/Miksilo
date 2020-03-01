
package core.parsers

import _root_.core.SourceUtils
import _root_.core.document.Empty
import _root_.core.parsers.core.{ParseText, Processor}
import _root_.core.parsers.editorParsers.LeftRecursiveCorrectingParserWriter
import _root_.core.parsers.strings.{CommonParserWriter, IndentationSensitiveParserWriter, WhitespaceParserWriter}
import _root_.core.responsiveDocument._
import org.scalatest.funsuite.AnyFunSuite

trait YamlExpression {
  def toDocument: ResponsiveDocument

  override def toString: String = toDocument.renderString()
}

case class Object(members: Map[YamlExpression, YamlExpression]) extends YamlExpression {

  override def toDocument: ResponsiveDocument = {
    members.
      map(member => member._1.toDocument ~ ":" ~~ member._2.toDocument).
      reduce((t,b) => t % b)
  }
}

case class YamlArray(elements: Seq[YamlExpression]) extends YamlExpression {
  override def toDocument: ResponsiveDocument = {
    elements.
      map(member => ResponsiveDocument.text("- ") ~~ member.toDocument).
      fold[ResponsiveDocument](Empty)((t: ResponsiveDocument, b: ResponsiveDocument) => t % b)
  }

}

case class Number(value: Int) extends YamlExpression {
  override def toDocument: ResponsiveDocument = ResponsiveDocument.text(value.toString)
}

case class StringLiteral(value: String) extends YamlExpression {
  override def toDocument: ResponsiveDocument = ResponsiveDocument.text(value.toString)
}

trait YamlContext
object FlowIn extends YamlContext
object FlowOut extends YamlContext
object BlockIn extends YamlContext
object BlockOut extends YamlContext
object BlockKey extends YamlContext
object FlowKey extends YamlContext

class YamlTest extends AnyFunSuite
  with LeftRecursiveCorrectingParserWriter
  with IndentationSensitiveParserWriter with CommonParserWriter with WhitespaceParserWriter {

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
  case class TaggedNode(tag: String, node: YamlExpression) extends YamlExpression {
    override def toDocument: ResponsiveDocument = ResponsiveDocument.text("!") ~ tag ~~ node.toDocument
  }

  lazy val parseUntaggedFlowValue = parseBracketArray | parseStringLiteral
  lazy val parseFlowValue = (tag.option ~ parseUntaggedFlowValue).map(t => t._1.fold(t._2)(tag => TaggedNode(tag, t._2)))
  lazy val parseUntaggedValue = new Lazy(parseBracketArray | parseArray | parseNumber | parseStringLiteral | parseBlockMapping, "untagged value")
  lazy val parseValue: Parser[YamlExpression] = (tag.option ~ parseUntaggedValue).map(t => t._1.fold(t._2)(tag => TaggedNode(tag, t._2)))
  lazy val parseYaml = parseValue ~< trivias

  lazy val parseBlockMapping: Parser[YamlExpression] = {
    val member = new WithContext(_ =>
      BlockKey, parseFlowValue) ~< literalOrKeyword(":") ~ greaterThan(parseValue)
    alignedList(member).map(values => {
      Object(values.toMap)
    })
  }

  lazy val parseBracketArray: Parser[YamlExpression] = {
    val inner = "[" ~> parseFlowValue.manySeparated(",", "array element").map(elements => YamlArray(elements)) ~< "]"
    new WithContext(_ => FlowIn, inner)
  }

  lazy val parseArray: Parser[YamlExpression] = {
    val element = literalOrKeyword("- ") ~> greaterThan(parseValue)
    alignedList(element).map(elements => YamlArray(elements))
  }

  lazy val parseNumber: Parser[YamlExpression] =
    wholeNumber.map(n => Number(Integer.parseInt(n)))

  lazy val parseStringLiteral: Parser[YamlExpression] =
    parseStringLiteralInner.map(s => StringLiteral(s))
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

  test("plainStyleMultineLineInFlowCollection") {
    val input = """                  [<img src=", !FindInMap [Region2Examples, !Ref 'AWS::Region',
                  |                                              Examples], /cloudformation_graphic.png" alt="AWS CloudFormation
                  |                                                           Logo"/>, '<h1>Congratulations, you have successfully launched
                  |                    the AWS CloudFormation sample.</h1>']""".stripMargin
    val result = parseValue.getWholeInputParser().resetAndParse(input)
    assert(result.successful)
  }

  test("tagged block key") {
    val input = """      UserData: !Base64
                  |        Fn::Join:
                  |          - ''
                  |          - ['#!/bin/bash -xe
                  |
                  |            ', 'yum update -y aws-cfn-bootstrap
                  |
                  |            ', '/opt/aws/bin/cfn-init -v ', '         --stack ', !Ref 'AWS::StackName',
                  |             '         --resource LaunchConfig ', '         --region ', !Ref 'AWS::Region',
                  |             '
                  |
                  |            ', '/opt/aws/bin/cfn-signal -e $? ', '         --stack ', !Ref 'AWS::StackName',
                  |             '         --resource WebServerGroup ', '         --region ', !Ref 'AWS::Region',
                  |             '
                  |
                  |            ']
                  |""".stripMargin
    val result = parseYaml.getWholeInputParser().resetAndParse(input)
    assert(result.successful)
  }


  test("string") {
    val program = "'hello'"

    val result = parseStringLiteral.getWholeInputParser().resetAndParse(program)
    assertResult(StringLiteral("hello"))(result.get)
  }

  test("number") {
    val program = "3"

    val result = parseNumber.getWholeInputParser().resetAndParse(program)
    assertResult(Number(3))(result.get)
  }

  test("object with single member") {
    val program =
      """minecraft: 2""".stripMargin

    val result = parseValue.getWholeInputParser().resetAndParse(program)
    val expectation = Object(Map(StringLiteral("minecraft") -> Number(2)))
    assertResult(expectation)(result.get)
  }

  test("object with 2 members") {
    val program =
      """minecraft: 2
        |cancelled: 3""".stripMargin

    val result = parseValue.getWholeInputParser().resetAndParse(program)
    val expectation = Object(Map(StringLiteral("minecraft") -> Number(2), StringLiteral("cancelled") -> Number(3)))
    assertResult(expectation)(result.get)
  }

  test("array") {
    val program =
      """- 2
        |- 3""".stripMargin

    val result = parseValue.getWholeInputParser().resetAndParse(program)
    val expectation = YamlArray(Seq(Number(2), Number(3)))
    assertResult(expectation)(result.get)
  }

  test("object nested in singleton array") {
    val program =
      """- x: 3
        |  y: 4""".stripMargin

    val result = parseValue.getWholeInputParser().resetAndParse(program)
    val expectation = YamlArray(Seq(Object(Map(StringLiteral("x") -> Number(3), StringLiteral("y") -> Number(4)))))
    assertResult(expectation)(result.get)
  }

  test("array object composite 2") {
    val program =
      """- x: 3
        |  y: 4
        |- 2""".stripMargin

    val result = parseValue.getWholeInputParser().resetAndParse(program)
    val expectation = YamlArray(Seq(Object(Map(StringLiteral("x") -> Number(3), StringLiteral("y") -> Number(4))), Number(2)))
    assertResult(expectation)(result.get)
  }

  test("array object composite") {
    val program =
      """- 2
        |- x: 3
        |  y: 4""".stripMargin

    val result = parseValue.getWholeInputParser().resetAndParse(program)
    val expectation = YamlArray(Seq(Number(2), Object(Map(StringLiteral("x") -> Number(3), StringLiteral("y") -> Number(4)))))
    assertResult(expectation)(result.get)
  }

  test("complex composite 2") {
    val program =
      """- a: - 1
        |- b: - 2""".stripMargin

    val result = parseValue.getWholeInputParser().resetAndParse(program)
    val expectation = YamlArray(Seq(
      Object(Map(
        StringLiteral("a") -> YamlArray(Seq(Number(1))))),
      Object(Map(
        StringLiteral("b") -> YamlArray(Seq(Number(2)))))
    ))
    assertResult(expectation)(result.get)
  }

  test("complex composite 3") {
    val program =
      """- 2
        |- x: 3
        |  y: a: 4
        |     b: 5
        |  z: - 2
        |     - 4
        |- 6
        |- q: - 7
        |     - 8
        |  r: 9""".stripMargin

    val result = parseValue.getWholeInputParser().resetAndParse(program)
    val expectation =
      YamlArray(Seq(
        Number(2),
        Object(Map(StringLiteral("x") -> Number(3),
          StringLiteral("y") -> Object(Map(StringLiteral("a") -> Number(4), StringLiteral("b") -> Number(5))),
          StringLiteral("z") -> YamlArray(Seq(Number(2), Number(4))))),
        Number(6),
        Object(Map(
          StringLiteral("q") ->
            YamlArray(Seq(Number(7), Number(8))),
          StringLiteral("r") -> Number(9)))))
    assertResult(expectation)(result.get)
  }

  test("big yaml file") {
    val contents = SourceUtils.getResourceFileContents("AutoScalingMultiAZWithNotifications.yaml")
    val result = parseValue.getWholeInputParser().resetAndParse(contents)
    assert(result.successful, result.toString)
  }
}
