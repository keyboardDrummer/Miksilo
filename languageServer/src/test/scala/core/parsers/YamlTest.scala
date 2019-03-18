
package core2.parsers

import core.document.Empty
import core.parsers.editorParsers.{DefaultCache, UnambiguousEditorParserWriter}
import core.parsers.strings.{CommonParserWriter, IndentationSensitiveParserWriter}
import core.responsiveDocument.ResponsiveDocument
import langserver.types.Position
import org.scalatest.FunSuite
import util.SourceUtils

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

class YamlTest extends FunSuite
  with UnambiguousEditorParserWriter
  with IndentationSensitiveParserWriter with CommonParserWriter {

  type Input = IndentationReader

  trait YamlContext
  object FlowIn extends YamlContext
  object FlowOut extends YamlContext
  object BlockIn extends YamlContext
  object BlockOut extends YamlContext
  object BlockKey extends YamlContext
  object FlowKey extends YamlContext

  class IndentationReader(array: ArrayCharSequence, offset: Int, position: Position, val context: YamlContext, val indentation: Int)
    extends StringReaderBase(array, offset, position) with IndentationReaderLike {

    override def withIndentation(value: Int) = new IndentationReader(array, offset, position, context, value)

    def withContext(newState: YamlContext): IndentationReader = new IndentationReader(array, offset, position, newState, indentation)

    def this(text: String) {
      this(text.toCharArray, 0, Position(0, 0), BlockOut, 0)
    }

    override def drop(amount: Int) = new IndentationReader(array, offset + amount,
      newPosition(position, array, offset, amount), context, indentation)

    override def hashCode(): Int = offset ^ indentation ^ context.hashCode()

    override def equals(obj: Any): Boolean = obj match {
      case other: IndentationReader => offset == other.offset && indentation == other.indentation && context == other.context
      case _ => false
    }
  }

  val whiteSpace = RegexParser("""\s*""".r)
  override def leftRight[Left, Right, NewResult](left: EditorParser[Left],
                                                 right: => EditorParser[Right],
                                                 combine: (Left, Right) => NewResult): EditorParser[NewResult] = {
    new Sequence(left, new Sequence(whiteSpace, right, (a: String, b: Right) => b), combine)
  }

  class IfContext[Result](inners: Map[YamlContext, EditorParser[Result]]) extends EditorParser[Result] {
    override def parseInternal(input: IndentationReader, state: ParseStateLike) = {
      inners(input.context).parseInternal(input, state)
    }

    override def getDefault(cache: DefaultCache) =
      inners.values.flatMap(inner => inner.getDefault(cache)).headOption
  }

  class WithContext[Result](update: YamlContext => YamlContext, inner: EditorParser[Result]) extends EditorParser[Result] {
    override def parseInternal(input: IndentationReader, state: ParseStateLike) = {
      val result = inner.parseInternal(input.withContext(update(input.context)), state)
      result.updateRemainder(r => r.withContext(input.context))
    }

    override def getDefault(cache: DefaultCache) = inner.getDefault(cache)
  }

  lazy val tag: EditorParser[String] = "!" ~> RegexParser("""[^'\n !,\[\]{}]+""".r) //Should be 	ns-uri-char - “!” - c-flow-indicator
  case class TaggedNode(tag: String, node: YamlExpression) extends YamlExpression {
    override def toDocument: ResponsiveDocument = ResponsiveDocument.text("!") ~ tag ~~ node.toDocument
  }

  lazy val parseUntaggedFlowValue = parseBracketArray | parseStringLiteral
  lazy val parseFlowValue = (tag.option ~ parseUntaggedFlowValue).map(t => t._1.fold(t._2)(tag => TaggedNode(tag, t._2)))
  lazy val parseUntaggedValue = lazyParser(parseBracketArray | parseArray | parseNumber | parseStringLiteral | parseBlockMapping)
  lazy val parseValue: EditorParser[YamlExpression] = (tag.option ~ parseUntaggedValue).map(t => t._1.fold(t._2)(tag => TaggedNode(tag, t._2)))

  lazy val parseBlockMapping: EditorParser[YamlExpression] = {
    val member = new WithContext(_ =>
      BlockKey, parseFlowValue) ~< literal(":") ~ greaterThan(parseValue)
    alignedList(member).map(values => {
      Object(values.toMap)
    })
  }

  lazy val parseBracketArray: EditorParser[YamlExpression] = {
    val inner = "[" ~> parseFlowValue.manySeparated(",").map(elements => Array(elements)) ~< "]"
    new WithContext(_ => FlowIn, inner)
  }

  lazy val parseArray: EditorParser[YamlExpression] = {
    val element = literal("- ") ~> greaterThan(parseValue)
    alignedList(element).map(elements => Array(elements))
  }

  lazy val parseNumber: EditorParser[YamlExpression] =
    wholeNumber.map(n => Number(Integer.parseInt(n)))

  lazy val parseStringLiteral: EditorParser[YamlExpression] =
    parseStringLiteralInner.map(s => StringLiteral(s))
  lazy val parseStringLiteralInner: EditorParser[String] =
    regex("""'[^']*'""".r).map(n => n.drop(1).dropRight(1)) | plainScalar


  lazy val plainScalar = new WithContext({
    case FlowIn => FlowIn
    case BlockKey => BlockKey
    case FlowKey => FlowKey
    case _ => FlowOut
  }, plainStyleMultiLineString | plainStyleSingleLineString)

  val nbChars = "\n"
  val nsChars = nbChars + " "
  val flowIndicatorChars = ",[]{}"

  val nsPlainSafeIn =  RegexParser("""([^\n:#'\[\]{},]|:[^\n# '\[\]{},])+""".r) //s"[^$nbChars$flowIndicatorChars]*]".r)
  val nsPlainSafeOut =  RegexParser("""([^\n':#]|:[^\n #'])+""".r) //s"[^$nbChars]*]".r)

  val nsPlainSafe = new IfContext(Map(
    FlowIn -> nsPlainSafeIn,
    FlowOut -> nsPlainSafeOut,
    BlockKey -> nsPlainSafeOut,
    FlowKey -> nsPlainSafeIn))

  lazy val plainStyleSingleLineString = nsPlainSafe
  lazy val plainStyleMultiLineString = new Sequence(new Sequence(nsPlainSafe, whiteSpace, (l: String, r: String) => l),
    greaterThan(WithIndentation(equal(nsPlainSafe).manySeparated("\n"))),
    (firstLine: String, rest: List[String]) => {
      firstLine + rest.fold("")((a,b) => a + " " + b)
  })

  test("plainStyleMultineLineInFlowCollection") {
    val input =
      """/cloudformation_graphic.png" alt="AWS CloudFormation
        |         Logo"/""".stripMargin
    val result = plainStyleMultiLineString.parse(new IndentationReader(input).withContext(FlowIn))
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

    val result = parseStringLiteral.parse(new IndentationReader(program))
    assertResult(StringLiteral("hello"))(result.get)
  }

  test("number") {
    val program = "3"

    val result = parseNumber.parse(new IndentationReader(program))
    assertResult(Number(3))(result.get)
  }
//
//  test("inline member") {
//    val key = FromLinearParser(ident <~ literal(": "))
//    val member: GridParser[Char, (String, YamlExpression)] = key ~ parseNumber
//    val program = "hallo: 3"
//    val result = member.parseEntireGrid(program)
//    assertResult(ParseSuccess(Size(8, 1), ("hallo", Number(3))))(result)
//  }
//
//  test("inline members") {
//    val key = FromLinearParser(ident <~ literal(": "))
//    val member: GridParser[Char, (String, YamlExpression)] = (key ~ parseNumber).name("member")
//
//    val program =
//      minecraft: 2
//        |cancelled: 3""".stripMargin
//    val result = member.someVertical.parseEntireGrid(program)
//    assertResult(ParseSuccess(Size(12, 2), List("minecraft" -> Number(2), "cancelled" -> Number(3))))(result)
//  }
//

  test("object with single member") {
    val program =
      """minecraft: 2""".stripMargin

    val result = parseValue.parse(new IndentationReader(program))
    val expectation = Object(Map(StringLiteral("minecraft") -> Number(2)))
    assertResult(expectation)(result.get)
  }

  test("object with 2 members") {
    val program =
      """minecraft: 2
        |cancelled: 3""".stripMargin

    val result = parseValue.parse(new IndentationReader(program))
    val expectation = Object(Map(StringLiteral("minecraft") -> Number(2), StringLiteral("cancelled") -> Number(3)))
    assertResult(expectation)(result.get)
  }
//
//  test("complex failure") {
//    val program =
//      """a:
//        | b: CannotParse
//        |c: 4""".stripMargin
//
//    val result = parseValue.parseEntireGrid(program)
//    val failureLocation = result.asInstanceOf[ParseFailure[YamlExpression]].absoluteLocation
//    assertResult(Location(1, 15))(failureLocation)
//  }
//
//  test("complex failure 2") {
//    val program =
//      """a:
//        |   b: /
//        |c:
//        |   4""".stripMargin
//
//    val result = parseValue.parseEntireGrid(program)
//    val failureLocation = result.asInstanceOf[ParseFailure[YamlExpression]].absoluteLocation
//    assertResult(Location(1, 6))(failureLocation)
//  }
//
//  test("array failure") {
//    val program =
//      """- 2
//        |- 3
//        |x""".stripMargin
//
//    val result = parseValue.parseEntireGrid(program)
//    val expectation = ("`- ' expected but `x' found", Location(2,0))
//    assertResult(expectation)(result.testProperties)
//
//    val partialResult = parseValue.parse(program).asInstanceOf[ParseSuccess[YamlExpression]]
//    val partialExpectation = ParseSuccess(Size(3, 2), Array(Seq(Number(2), Number(3))))
//    assertResult(partialExpectation)(ParseSuccess(partialResult.size, partialResult.result))
//  }
//
  test("array") {
    val program =
      """- 2
        |- 3""".stripMargin

    val result = parseValue.parse(new IndentationReader(program))
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
//
//  test("array object composite 2") {
//    val program =
//      """- x: 3
//        |  y: 4
//        |- 2""".stripMargin
//
//    val result = parseValue.parseEntireGrid(program)
//    val expectation = ParseSuccess(Size(6, 3), Array(Seq(Object(Map("x" -> Number(3), "y" -> Number(4))), Number(2))))
//    assertResult(expectation)(result)
//  }
//
//  test("array object composite") {
//    val program =
//      """- 2
//        |- x: 3
//        |  y: 4""".stripMargin
//
//    val result = parseValue.parseEntireGrid(program)
//    val expectation = ParseSuccess(Size(6, 3), Array(Seq(Number(2), Object(Map("x" -> Number(3), "y" -> Number(4))))))
//    assertResult(expectation)(result)
//  }
//
  test("complex composite 2") {
    val program =
      """- a: - 1
        |- b: - 2""".stripMargin

    val result = parseValue.parse(new IndentationReader(program))
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

    val result = parseValue.parse(new IndentationReader(program))
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
