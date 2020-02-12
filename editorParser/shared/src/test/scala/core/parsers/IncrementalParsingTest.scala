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
    parser.insertRange(1,14, input2.toCharArray)
    val result3 = parser.parse(input2)
    assertResult(List("hello" -> "bla"))(result3.resultOption.get)
  }

  test("removing works") {
    val input = """{"hello":"bla"}"""
    val input2 = """{}"""
    val parser = getParser
    val result = parser.parse(input)
    parser.removeRange(1,14, input2.toCharArray)
    val result3 = parser.parse(input2)
    assertResult(List())(result3.resultOption.get)
  }

  test("partial clearing works") {
    val input = """{"foo":"bar"}"""
    val input2 = """{"bar":"foo"}"""
    val parser = getParser
    parser.parse(input)
    parser.removeRange(1,4, input2.toCharArray)
    parser.insertRange(1,4, input2.toCharArray)
    val result = parser.parse(input2)
    assertResult(List("bar" -> "bar"))(result.resultOption.get)
  }

  test("inserts work") {
    val input = """[1,3]"""
    val input2 = """[1,2,4]"""
    val parser = getParser
    val result1 = parser.parse(input)
    parser.insertRange(2,4, input2.toCharArray)
    val result = parser.parse(input2)
    assertResult(List(1,2,3))(result.resultOption.get)
  }

  def getParser = {
    jsonParser.getSingleResultParser
  }

}
