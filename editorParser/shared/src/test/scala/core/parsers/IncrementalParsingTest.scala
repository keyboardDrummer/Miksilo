package core.parsers

import org.scalatest.funsuite.AnyFunSuite

class IncrementalParsingTest extends AnyFunSuite {

  import ParseJson._

  test("inserting between two empty braces works") {
    val input = """{}"""
    val input2 = """{"hello":"bla"}"""
    val parser = getParser
    val result = parser.parse(input)
    val result2 = parser.parse(input2)
    assertResult(result)(result2)
    parser.changeRange(1, 1, 13)
    val result3 = parser.parse(input2)
    assertResult(List("hello" -> "bla"))(result3.resultOption.get)
  }

  test("removing works") {
    val input = """{"hello":"bla"}"""
    val input2 = """{}"""
    val parser = getParser
    val result = parser.parse(input)
    parser.changeRange(1,14, 0)
    val result3 = parser.parse(input2)
    assertResult(List())(result3.resultOption.get)
  }

  test("partial clearing works") {
    val input = """{"foo":"bar"}"""
    val input2 = """{"bar":"foo"}"""
    val parser = getParser
    parser.parse(input)
    parser.changeRange(1,4, 3)
    val result = parser.parse(input2)
    assertResult(List("bar" -> "bar"))(result.resultOption.get)
  }

  test("inserts work") {
    val input = """[1,3]"""
    val input2 = """[0,2,0]"""
    val parser = getParser
    parser.parse(input)
    parser.changeRange(2,2,2)
    val result = parser.parse(input2)
    assertResult(List(1,2,3))(result.resultOption.get)
  }

  test("multiple inserts work") {
    val input = """[1,3,5]"""
    val input2 = """[0,2,0,0]"""
    val input3 = """[0,2,0,4,0]"""
    val parser = getParser
    parser.parse(input)
    parser.changeRange(2,2,2)
    parser.changeRange(6,6,2)
    val result = parser.parse(input3)
    assertResult(List(1,2,3,4,5))(result.resultOption.get)
  }

  test("inserting at the end workds") {
    val input = """1"""
    val input2 = """135"""
    val parser = getParser
    parser.parse(input)
    parser.changeRange(1,1,2)
    val result = parser.parse(input2)
    assertResult(135)(result.resultOption.get)
  }

  // TODO add tests with linebreaks.

  // TODO add test that checks whether positions in the AST have been updated.

  // TODO add test with indentation sensitive parsing.

  def getParser = {
    jsonParser.getSingleResultParser
  }

}
