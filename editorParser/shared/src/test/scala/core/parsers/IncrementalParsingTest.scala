package core.parsers

import org.scalatest.funsuite.AnyFunSuite

class IncrementalParsingTest extends AnyFunSuite {

  import ParseJson._

  test("clearing between two empty braces works") {
    val input = """{}"""
    val input2 = """{"hello":"bla"}"""
    val parser = getParser
    val result = parser.parse(new StringReader(input))
    val result2 = parser.parse(new StringReader(input2))
    assertResult(result)(result2)
    parser.applyChange(1,1)
    val result3 = parser.parse(new StringReader(input2))
    assertResult(List("hello" -> "bla"))(result3.resultOption.get)
  }

  def getParser = {
    jsonParser.getWholeInputParser
  }

}
