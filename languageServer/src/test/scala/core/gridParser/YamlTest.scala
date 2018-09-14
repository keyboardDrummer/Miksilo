package core.gridParser

import core.gridParser.grids.GridFromString
import core.responsiveDocument.ResponsiveDocument
import org.scalatest.FunSuite
import util.SourceUtils

import scala.util.parsing.combinator.JavaTokenParsers

class YamlTest extends FunSuite with RegexGridParsers with JavaTokenParsers {

  trait YamlExpression {
    def toDocument: ResponsiveDocument

    override def toString: String = toDocument.renderString()
  }

//  var tokens = [
//  ['comment', /^#[^\n]*/],
//  ['indent', /^\n( *)/],
//  ['space', /^ +/],
//  ['true', /^\b(enabled|true|yes|on)\b/],
//  ['false', /^\b(disabled|false|no|off)\b/],
//  ['null', /^\b(null|Null|NULL|~)\b/],
//  ['string', /^"(.*?)"/],
//  ['string', /^'(.*?)'/],
//  ['timestamp', /^((\d{4})-(\d\d?)-(\d\d?)(?:(?:[ \t]+)(\d\d?):(\d\d)(?::(\d\d))?)?)/],
//  ['float', /^(\d+\.\d+)/],
//  ['int', /^(\d+)/],
//  ['doc', /^---/],
//  [',', /^,/],
//  ['{', /^\{(?![^\n\}]*\}[^\n]*[^\s\n\}])/],
//['}', /^\}/],
//['[', /^\[(?![^\n\]]*\][^\n]*[^\s\n\]])/],
//[']', /^\]/],
//['-', /^\-/],
//[':', /^[:]/],
//['string', /^(?![^:\n\s]*:[^\/]{2})(([^:,\]\}\n\s]|(?!\n)\s(?!\s*?\n)|:\/\/|,(?=[^\n]*\s*[^\]\}\s\n]\s*\n)|[\]\}](?=[^\n]*\s*[^\]\}\s\n]\s*\n))*)(?=[,:\]\}\s\n]|$)/],
//  ['id', /^([\w][\w -]*)/]
//  ]

  case class Object(members: Map[String, YamlExpression]) extends YamlExpression {

    override def toDocument: ResponsiveDocument = {
      members.
        map(member => ResponsiveDocument.text(member._1) ~ ":" ~~ member._2.toDocument).
        reduce((t,b) => t % b)
    }
  }

  case class Array(elements: Seq[YamlExpression]) extends YamlExpression {
    override def toDocument: ResponsiveDocument = {
      elements.
        map(member => ResponsiveDocument.text("- ") ~~ member.toDocument).
        reduce((t,b) => t % b)
    }

  }

  case class Number(value: Int) extends YamlExpression {
    override def toDocument: ResponsiveDocument = ResponsiveDocument.text(value.toString)
  }

  case class StringLiteral(value: String) extends YamlExpression {
    override def toDocument: ResponsiveDocument = ResponsiveDocument.text(value.toString)
  }

  lazy val parseValue: GridParser[Char, YamlExpression] = parseArray.name("array") | parseObject.name("object") |
    parseNumber | parseStringLiteral

  lazy val parseObject: GridParser[Char, YamlExpression] = {
    val key = FromLinearParser(ident <~ literal(":"))
    val lrMember = (FromLinearParser(ident <~ literal(":")) %< Indent(1, canBeWider = false, mustParseSomething = false)) ~
      (Indent(1, canBeWider = true, mustParseSomething = true) ~> parseValue)
    val member: GridParser[Char, (String, YamlExpression)] = lrMember.name("lrMember") | (key % parseValue.indent(1).name("objectPropValue"))
    member.someVertical.map(values => Object(values.toMap))
  }

  lazy val parseArray: GridParser[Char, YamlExpression] = {
    val element = (FromLinearParser(literal("- ")) %< Indent(1, canBeWider = true, mustParseSomething = false)) ~> parseValue
    element.someVertical.map(elements => Array(elements))
  }

  lazy val parseNumber: GridParser[Char, YamlExpression] =
    FromLinearParser(wholeNumber).map(n => Number(Integer.parseInt(n)))

  lazy val parseStringLiteral: GridParser[Char, YamlExpression] =
    FromLinearParser(regex("""'(.*?)'""".r)).
    //FromLinearParser(regex("""/^(?![^:\\n\\s]*:[^\\/]{2})(([^:,\\]\\}\\n\\s]|(?!\\n)\\s(?!\\s*?\\n)|:\\/\\/|,(?=[^\\n]*\\s*[^\\]\\}\\s\\n]\\s*\\n)|[\\]\\}](?=[^\\n]*\\s*[^\\]\\}\\s\\n]\\s*\\n))*)(?=[,:\\]\\}\\s\\n]|$)/""".r)).
      map(n => StringLiteral(n))

  implicit def toGrid(value: String): GridFromString = GridFromString(value)

  test("string") {
    val program = "'hello'"

    val result = parseStringLiteral.parseEntireGrid(program)
    val expectation = ParseSuccess(Size(7, 1), StringLiteral("'hello'"))
    assertResult(expectation)(result)
  }

  test("number") {
    val program = "3"

    val result = parseNumber.parseEntireGrid(program)
    val expectation = ParseSuccess(Size(1, 1), Number(3))
    assertResult(expectation)(result)
  }

  test("inline member") {
    val key = FromLinearParser(ident <~ literal(": "))
    val member: GridParser[Char, (String, YamlExpression)] = key ~ parseNumber
    val program = "hallo: 3"
    val result = member.parseEntireGrid(program)
    assertResult(ParseSuccess(Size(8, 1), ("hallo", Number(3))))(result)
  }

  test("inline members") {
    val key = FromLinearParser(ident <~ literal(": "))
    val member: GridParser[Char, (String, YamlExpression)] = (key ~ parseNumber).name("member")

    val program =
      """minecraft: 2
        |cancelled: 3""".stripMargin
    val result = member.someVertical.parseEntireGrid(program)
    assertResult(ParseSuccess(Size(12, 2), List("minecraft" -> Number(2), "cancelled" -> Number(3))))(result)
  }

  test("object with 2 members") {
    val program =
      """minecraft: 2
        |cancelled: 3""".stripMargin

    val result = parseValue.parseEntireGrid(program)
    val expectation = ParseSuccess(Size(12, 2), Object(Map("minecraft" -> Number(2), "cancelled" -> Number(3))))
    assertResult(expectation)(result)
  }

  test("complex failure") {
    val program =
      """a:
        | b: CannotParse
        |c: 4""".stripMargin

    val result = parseValue.parseEntireGrid(program)
    val failureLocation = result.asInstanceOf[ParseFailure[YamlExpression]].absoluteLocation
    assertResult(Location(1, 15))(failureLocation)
  }

  test("complex failure 2") {
    val program =
      """a:
        |   b: /
        |c:
        |   4""".stripMargin

    val result = parseValue.parseEntireGrid(program)
    val failureLocation = result.asInstanceOf[ParseFailure[YamlExpression]].absoluteLocation
    assertResult(Location(1, 6))(failureLocation)
  }

  test("array failure") {
    val program =
      """- 2
        |- 3
        |x""".stripMargin

    val result = parseValue.parseEntireGrid(program)
    val expectation = ("`- ' expected but `x' found", Location(2,0))
    assertResult(expectation)(result.testProperties)

    val partialResult = parseValue.parse(program).asInstanceOf[ParseSuccess[YamlExpression]]
    val partialExpectation = ParseSuccess(Size(3, 2), Array(Seq(Number(2), Number(3))))
    assertResult(partialExpectation)(ParseSuccess(partialResult.size, partialResult.result))
  }

  test("array") {
    val program =
      """- 2
        |- 3""".stripMargin

    val result = parseValue.parseEntireGrid(program)
    val expectation = ParseSuccess(Size(3, 2), Array(Seq(Number(2), Number(3))))
    assertResult(expectation)(result)
  }

  test("object nested in singleton array") {
    val program =
      """- x: 3
        |  y: 4""".stripMargin

    val result = parseValue.parseEntireGrid(program)
    val expectation = ParseSuccess(Size(6, 2), Array(Seq(Object(Map("x" -> Number(3), "y" -> Number(4))))))
    assertResult(expectation)(result)
  }

  test("array object composite 2") {
    val program =
      """- x: 3
        |  y: 4
        |- 2""".stripMargin

    val result = parseValue.parseEntireGrid(program)
    val expectation = ParseSuccess(Size(6, 3), Array(Seq(Object(Map("x" -> Number(3), "y" -> Number(4))), Number(2))))
    assertResult(expectation)(result)
  }

  test("array object composite") {
    val program =
      """- 2
        |- x: 3
        |  y: 4""".stripMargin

    val result = parseValue.parseEntireGrid(program)
    val expectation = ParseSuccess(Size(6, 3), Array(Seq(Number(2), Object(Map("x" -> Number(3), "y" -> Number(4))))))
    assertResult(expectation)(result)
  }

  test("complex composite 2") {
    val program =
      """- a: - 1
        |- b: - 2""".stripMargin

    val result = parseValue.parseEntireGrid(program)
    val expectation = ParseSuccess(Size(8, 2),
      Array(Seq(
        Object(Map(
          "a" -> Array(Seq(Number(1))))),
        Object(Map(
          "b" -> Array(Seq(Number(2)))))
      )))
    assertResult(expectation)(result)
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

    val result = parseValue.parseEntireGrid(program)
    val expectation = ParseSuccess(Size(9, 10),
      Array(Seq(
        Number(2),
          Object(Map("x" -> Number(3),
            "y" -> Object(Map("a" -> Number(4), "b" -> Number(5))),
            "z" -> Array(Seq(Number(2), Number(4))))),
        Number(6),
        Object(Map(
          "q" ->
            Array(Seq(Number(7), Number(8))),
          "r" -> Number(9))))))
    assertResult(expectation)(result)
  }

  test("big yaml file") {
    val contents = SourceUtils.getTestFileContents("AutoScalingMultiAZWithNotifications.yaml")
    val result = parseValue.parseEntireGrid(contents)
    assert(result.isInstanceOf[ParseSuccess[YamlExpression]], result.toString)
  }
}
