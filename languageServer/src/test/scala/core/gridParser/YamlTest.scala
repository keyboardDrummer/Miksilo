package core.gridParser

import core.gridParser.grids.GridFromString
import org.scalatest.FunSuite

class YamlTest extends FunSuite {

  trait YamlExpression
  case class Object(members: Map[String, YamlExpression]) extends YamlExpression
  case class Array(elements: Seq[YamlExpression]) extends YamlExpression
  case class Number(value: Int) extends YamlExpression

  lazy val parseValue: GridParser[Char, YamlExpression] = parseObject | parseArray | parseNumber

  lazy val parseObject: GridParser[Char, YamlExpression] = {
    val key = Row(CharParsers.ident <~ CharParsers.literal(":"))
    val member: GridParser[Char, (String, YamlExpression)] = key ~ parseValue | key % parseValue.indent(1)
    member.someVertical.map(values => Object(values.toMap))
  }

  lazy val parseArray: GridParser[Char, YamlExpression] = {
    val element = Row(CharParsers.literal("- ")) ~> parseValue
    element.someVertical.map(elements => Array(elements))
  }

  lazy val parseNumber: GridParser[Char, YamlExpression] =
    Row(CharParsers.wholeNumber).map(n => Number(Integer.parseInt(n)))

  implicit def toGrid(value: String): GridFromString = GridFromString(value)

  test("number") {
    val program = "3"

    val result = parseNumber.parse(program)
    val expectation = ParseSuccess(Size(0, 1), Number(3))
    assertResult(expectation)(result)
  }

  test("inline member") {
    val key = Row(CharParsers.ident <~ CharParsers.literal(":"))
    val member: GridParser[Char, (String, YamlExpression)] = key ~ parseNumber
    val program = "hallo: 3"
    val result = member.parse(program)
    assertResult(ParseSuccess(Size(8, 0), ("hallo", Number(3))))(result)
  }

  test("inline members") {
    val key = Row(CharParsers.ident <~ CharParsers.literal(":"))
    val member: GridParser[Char, (String, YamlExpression)] = (key ~ parseNumber).name("member")

    val program =
      """minecraft: 2
        |cancelled: 3""".stripMargin
    val result = member.someVertical.parse(program)
    assertResult(ParseSuccess(Size(12, 2), List("minecraft" -> Number(2), "cancelled" -> Number(3))))(result)
  }

  test("object with 2 members") {
    val program =
      """minecraft: 2
        |cancelled: 3""".stripMargin

    val result = parseValue.parse(program)
    val expectation = ParseSuccess(Size(12, 2), Object(Map("minecraft" -> Number(2), "cancelled" -> Number(3))))
    assertResult(expectation)(result)
  }
}
