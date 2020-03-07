package core.parsers

import _root_.core.parsers.core.ParseText
import languages.json.JsonParser
import org.scalatest.funsuite.AnyFunSuite

class IncrementalParsingTest extends AnyFunSuite {

  test("inserting between two empty braces works") {
    val input = """{}"""
    val change = getChange
    val result1 = change(0,0, input)
    val result3 = change(1,1, """"hello":"bla"""")
    assertResult(List("hello" -> "bla"))(result3)
  }

  test("removing works") {
    val input = """{"hello":"bla"}"""
    val input2 = """{}"""
    val change = getChange
    change(0,0, input)
    val result3 = change(1,14, "")
    assertResult(List())(result3)
  }

  test("partial clearing works") {
    val input = """{"foo":"bar"}"""
    val input2 = """{"bar":"foo"}"""
    val change = getChange
    change(0,0, input)
    change.setText(input2)
    val result = change(2,5, "bar")
    assertResult(List("bar" -> "bar"))(result)
  }

  test("inserts work") {
    val input = """[1,3]"""
    val input2 = """[1,2,0]"""
    val change = getChange
    change(0,0,input)
    change.setText(input2)
    val result = change(2,2, ",2")
    assertResult(Array(1,2,3))(result)
  }

  test("multiple inserts work") {
    val input = """[1,3,5]"""
    val input2 = """[1,2,0,0]"""
    val input3 = """[0,2,3,4,0]"""
    val change = getChange
    change(0,0,"[1,3,5]")
    change.setText(input2)
    change(2,2,",2")
    change.setText(input3)
    val result = change(6,6,",4")
    assertResult(Array(1,2,3,4,5))(result)
  }

  test("inserting at the end workds") {
    val change = getChange
    change(0,0,"1")
    val result = change(1,1,"35")
    assertResult(135)(result)
  }

  test("success parser caching") {
    val change = getChange
    change(0,0, "[]")
    change(1,1, "{")
    val result = change(2,2, "}")
    assertResult(Array(List.empty))(result)
  }

  test("regression") {
    val change = getChange
    change(0,0, "[{}]")
    change(2,3, "")
    val result3 = change(2,2, "}")
    assertResult(List(List.empty))(result3)
  }

  test("regression2") {
    val change = getChange
    change(0, 0, "[]")
    change(1, 1, "{")
    change(2, 2, "}")
    change(2, 3, "")
    val result = change(2,2, "}")
    assertResult(List(List.empty))(result)
  }

  def getChange: Change = {
    val text = new ParseText()
    val parser = JsonParser.getParser(text)
    new Change {

      override def apply(from: Int, until: Int, newText: String) = {
        text.applyRangeChange(newText = newText, start = from, end = until)
        parser.changeRange(from, until, newText.length)
        JsonTestUtils.valueToPrimitive(parser.parse().resultOption.get)
      }

      override def setText(newText: String): Unit = {
        text.arrayOfChars = newText.toCharArray
      }
    }
  }

  trait Change {
    def apply(from: Int, until: Int, newText: String): Any
    def setText(newText: String): Unit
  }

  // TODO add tests with linebreaks.

  // TODO add test that checks whether positions in the AST have been updated.

  // TODO add test with indentation sensitive parsing.

}
