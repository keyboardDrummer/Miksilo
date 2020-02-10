package core.parsers

import org.scalatest.funsuite.AnyFunSuite

class IncrementalParsingTest extends AnyFunSuite {

  import ParseJson._

  test("clearing between two empty braces works") {
    val input = """{}"""
    val input2 = """{"hello":"bla"}"""
    val parser = getParser
    val result = parser.parse(input)
    val result2 = parser.parse(input2)
    assertResult(result)(result2)
    parser.applyChange(1,1)
    val result3 = parser.parse(input2)
    assertResult(List("hello" -> "bla"))(result3.resultOption.get)
  }

  test("partial clearing works") {
    val input = """{"foo":"bar"}"""
    val input2 = """{"bar":"foo"}"""
    val parser = getParser
    parser.parse(input)
    parser.applyChange(7,12)
    val result = parser.parse(input2)
    assertResult(List("foo" -> "foo"))(result.resultOption.get)
  }
  // TODO add test where the text moves because of an insertion/deletion, and the cache indices should be updated.

  def getParser = {
    jsonParser.getWholeInputParser
  }

}
