
package core2.parsers

import core.document.Empty
import core.parsers.editorParsers.LeftRecursiveCorrectingParserWriter
import core.parsers.strings.{CommonParserWriter, IndentationSensitiveParserWriter}
import core.responsiveDocument.ResponsiveDocument
import org.scalatest.FunSuite
import util.SourceUtils
import langserver.types.Position

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

case class Array(elements: Seq[YamlExpression]) extends YamlExpression {
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

class YamlTest extends FunSuite
  with LeftRecursiveCorrectingParserWriter
  with IndentationSensitiveParserWriter with CommonParserWriter {

  type Input = IndentationReader

  class IndentationReader(array: ArrayCharSequence, offset: Int, position: Position, val context: YamlContext, val indentation: Int)
    extends StringReaderBase(array, offset, position) with IndentationReaderLike {

    override def withIndentation(value: Int) = new IndentationReader(array, offset, position, context, value)

    def withContext(newState: YamlContext): IndentationReader = new IndentationReader(array, offset, position, newState, indentation)

    def this(text: String) {
      this(text.toCharArray, 0, new Position(0,0), BlockOut, 0)
    }

    override def drop(amount: Int) = new IndentationReader(array, offset + amount, move(amount), context, indentation)

    override def hashCode(): Int = offset ^ indentation ^ context.hashCode()

    override def equals(obj: Any): Boolean = obj match {
      case other: IndentationReader => offset == other.offset && indentation == other.indentation && context == other.context
      case _ => false
    }
  }

  val whiteSpace = RegexParser("""\s*""".r, "whitespace")
  override def leftRight[Left, Right, NewResult](left: Self[Left],
                                                 right: => Self[Right],
                                                 combine: (Left, Right) => NewResult): Self[NewResult] = {
    new Sequence(left, new Sequence(whiteSpace, right, (a: String, b: Right) => b), combine)
  }

  class IfContext[Result](inners: Map[YamlContext, Self[Result]]) extends EditorParserBase[Result] {

    override def getParser(recursive: GetParse) = {
      val innerParsers = inners.mapValues(p => recursive(p))
      (input, state) => innerParsers(input.context)(input, state)
    }

    override def leftChildren = inners.values.toList

    override def getMustConsume(cache: ConsumeCache) = inners.values.forall(i => cache(i))

    override def children = leftChildren
  }

  class WithContext[Result](update: YamlContext => YamlContext, val original: Self[Result])
    extends EditorParserBase[Result] with ParserWrapper[Result] {

    override def getParser(recursive: GetParse): Parse[Result] = {
      val parseOriginal = recursive(original)

      def apply(input: IndentationReader, state: ParseState): ParseResult[Result] = {
        val context: YamlContext = input.context
        val result = parseOriginal(input.withContext(update(input.context)), state)
        result.updateRemainder(r => r.withContext(input.context))
      }

      apply
    }
  }

  val tag: Self[String] = "!" ~> RegexParser(s"""[^'\n !$flowIndicatorChars]+""".r, "tag name") //Should be 	ns-uri-char - “!” - c-flow-indicator
  case class TaggedNode(tag: String, node: YamlExpression) extends YamlExpression {
    override def toDocument: ResponsiveDocument = ResponsiveDocument.text("!") ~ tag ~~ node.toDocument
  }

  lazy val parseUntaggedFlowValue = parseBracketArray | parseStringLiteral
  lazy val parseFlowValue = (tag.option ~ parseUntaggedFlowValue).map(t => t._1.fold(t._2)(tag => TaggedNode(tag, t._2)))
  lazy val parseUntaggedValue = new Lazy(parseBracketArray | parseArray | parseNumber | parseStringLiteral | parseBlockMapping)
  lazy val parseValue: Self[YamlExpression] = (tag.option ~ parseUntaggedValue).map(t => t._1.fold(t._2)(tag => TaggedNode(tag, t._2)))

  lazy val parseBlockMapping: Self[YamlExpression] = {
    val member = new WithContext(_ =>
      BlockKey, parseFlowValue) ~< literal(":") ~ greaterThan(parseValue)
    alignedList(member).map(values => {
      Object(values.toMap)
    })
  }

  lazy val parseBracketArray: Self[YamlExpression] = {
    val inner = "[" ~> parseFlowValue.manySeparated(",", "array element").map(elements => Array(elements)) ~< "]"
    new WithContext(_ => FlowIn, inner)
  }

  lazy val parseArray: Self[YamlExpression] = {
    val element = literal("- ") ~> greaterThan(parseValue)
    alignedList(element).map(elements => Array(elements))
  }

  lazy val parseNumber: Self[YamlExpression] =
    wholeNumber.map(n => Number(Integer.parseInt(n)))

  lazy val parseStringLiteral: Self[YamlExpression] =
    parseStringLiteralInner.map(s => StringLiteral(s))
  lazy val parseStringLiteralInner: Self[String] =
    regex("""'[^']*'""".r, "single quote string literal").map(n => n.drop(1).dropRight(1)) | plainScalar


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
  val doubleColonPlainSafeIn =  RegexParser(s"""([^$plainSafeInChars:]|:[^$plainSafeInChars ])+""".r, "plain scalar")
  val doubleColonPlainSafeOut =  RegexParser(s"""([^$plainSafeOutChars:]|:[^$plainSafeOutChars ])+""".r, "plain scalar")

  val nsPlainSafe = new IfContext(Map(
    FlowIn -> doubleColonPlainSafeIn,
    FlowOut -> doubleColonPlainSafeOut,
    BlockKey -> doubleColonPlainSafeOut,
    FlowKey -> doubleColonPlainSafeIn))

  lazy val plainStyleSingleLineString = nsPlainSafe
  lazy val plainStyleMultiLineString = new Sequence(new Sequence(nsPlainSafe, whiteSpace, (l: String, _: String) => l),
    greaterThan(WithIndentation(equal(nsPlainSafe).manySeparated("\n", "line"))),
    (firstLine: String, rest: List[String]) => {
      firstLine + rest.fold("")((a,b) => a + " " + b)
  })

  test("plainStyleMultineLineInFlowCollection") {
    val input =
      """/cloudformation_graphic.png" alt="AWS CloudFormation
        |         Logo"/""".stripMargin
    val result = plainStyleMultiLineString.parseWholeInput(new IndentationReader(input).withContext(FlowIn))
    assert(result.successful)
  }

  test("plainStyleMultineLineInFlowCollection2") {
    val input = """                  [<img src=", !FindInMap [Region2Examples, !Ref 'AWS::Region',
                  |                                              Examples], /cloudformation_graphic.png" alt="AWS CloudFormation
                  |                                                           Logo"/>, '<h1>Congratulations, you have successfully launched
                  |                    the AWS CloudFormation sample.</h1>']""".stripMargin
    val result = parseValue.parseWholeInput(new IndentationReader(input))
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
    val result = parseValue.parseWholeInput(new IndentationReader(input))
    assert(result.successful)
  }


  test("string") {
    val program = "'hello'"

    val result = parseStringLiteral.parseWholeInput(new IndentationReader(program))
    assertResult(StringLiteral("hello"))(result.get)
  }

  test("number") {
    val program = "3"

    val result = parseNumber.parseWholeInput(new IndentationReader(program))
    assertResult(Number(3))(result.get)
  }

  test("object with single member") {
    val program =
      """minecraft: 2""".stripMargin

    val result = parseValue.parseWholeInput(new IndentationReader(program))
    val expectation = Object(Map(StringLiteral("minecraft") -> Number(2)))
    assertResult(expectation)(result.get)
  }

  test("object with 2 members") {
    val program =
      """minecraft: 2
        |cancelled: 3""".stripMargin

    val result = parseValue.parseWholeInput(new IndentationReader(program))
    val expectation = Object(Map(StringLiteral("minecraft") -> Number(2), StringLiteral("cancelled") -> Number(3)))
    assertResult(expectation)(result.get)
  }

  test("array") {
    val program =
      """- 2
        |- 3""".stripMargin

    val result = parseValue.parseWholeInput(new IndentationReader(program))
    val expectation = Array(Seq(Number(2), Number(3)))
    assertResult(expectation)(result.get)
  }

  test("object nested in singleton array") {
    val program =
      """- x: 3
        |  y: 4""".stripMargin

    val result = parseValue.parseWholeInput(new IndentationReader(program))
    val expectation = Array(Seq(Object(Map(StringLiteral("x") -> Number(3), StringLiteral("y") -> Number(4)))))
    assertResult(expectation)(result.get)
  }

  test("array object composite 2") {
    val program =
      """- x: 3
        |  y: 4
        |- 2""".stripMargin

    val result = parseValue.parseWholeInput(new IndentationReader(program))
    val expectation = Array(Seq(Object(Map(StringLiteral("x") -> Number(3), StringLiteral("y") -> Number(4))), Number(2)))
    assertResult(expectation)(result.get)
  }

  test("array object composite") {
    val program =
      """- 2
        |- x: 3
        |  y: 4""".stripMargin

    val result = parseValue.parseWholeInput(new IndentationReader(program))
    val expectation = Array(Seq(Number(2), Object(Map(StringLiteral("x") -> Number(3), StringLiteral("y") -> Number(4)))))
    assertResult(expectation)(result.get)
  }

  test("complex composite 2") {
    val program =
      """- a: - 1
        |- b: - 2""".stripMargin

    val result = parseValue.parseWholeInput(new IndentationReader(program))
    val expectation = Array(Seq(
      Object(Map(
        StringLiteral("a") -> Array(Seq(Number(1))))),
      Object(Map(
        StringLiteral("b") -> Array(Seq(Number(2)))))
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

    val result = parseValue.parseWholeInput(new IndentationReader(program))
    val expectation =
      Array(Seq(
        Number(2),
        Object(Map(StringLiteral("x") -> Number(3),
          StringLiteral("y") -> Object(Map(StringLiteral("a") -> Number(4), StringLiteral("b") -> Number(5))),
          StringLiteral("z") -> Array(Seq(Number(2), Number(4))))),
        Number(6),
        Object(Map(
          StringLiteral("q") ->
            Array(Seq(Number(7), Number(8))),
          StringLiteral("r") -> Number(9)))))
    assertResult(expectation)(result.get)
  }

  test("big yaml file") {
    val contents = SourceUtils.getTestFileContents("AutoScalingMultiAZWithNotifications.yaml")
    val result = parseValue.parseWholeInput(new IndentationReader(contents))
    assert(result.successful, result.toString)
  }
}
