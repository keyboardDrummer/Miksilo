package core.parsers

import _root_.core.parsers.core.ParseText
import _root_.core.parsers.editorParsers.SingleParseResult
import org.scalatest.funsuite.AnyFunSuite

class IncrementalParsingTest extends AnyFunSuite {

  import ParseJson._

  test("inserting between two empty braces works") {
    val input = """{}"""
    val input2 = """{}"""
    val change = getChange
    change(0,0,input)
    val result3 = change(1,1, """"hello":"bla"""")
    assertResult(List("hello" -> "bla"))(result3.resultOption.get)
  }

  test("removing works") {
    val input = """{"hello":"bla"}"""
    val input2 = """{}"""
    val change = getChange
    change(0,0, input)
    val result3 = change(1,14, "")
    assertResult(List())(result3.resultOption.get)
  }

  test("partial clearing works") {
    val input = """{"foo":"bar"}"""
    val input2 = """{"bar":"foo"}"""
    val change = getChange
    change(0,0, input)
    change.setText(input2)
    val result = chddddange(1,4, "bar")
    assertResult(List("bar" -> "bar"))(result.resultOption.get)
  }

  test("inserts work") {
    val input = """[1,3]"""
    val input2 = """[0,2,0]"""
    val change = getChange
    change(0,0,input)
    change.setText(input2)
    val result = change(2,2, "2,")
    assertResult(List(1,2,3))(result.resultOption.get)
  }

  test("multiple inserts work") {
    val input = """[1,3,5]"""
    val input2 = """[0,2,0,0]"""
    val input3 = """[0,2,0,4,0]"""
    val change = getChange
    change(0,0,"[1,3,5]")
    change.setText(input2)
    change(2,2,"2,")
    change.setText(input3)
    val result = change(6,6,"4,")
    assertResult(List(1,2,3,4,5))(result.resultOption.get)
  }

  test("inserting at the end workds") {
    val change = getChange
    change(0,0,"1")
    val result = change(1,1,"35")
    assertResult(135)(result.resultOption.get)
  }

  test("success parser caching") {
    val change = getChange
    change(0,0, "[]")
    change(1,1, "{")
    val result = change(2,2, "}")
    assertResult(List(List.empty))(result.resultOption.get)
  }

  test("regression") {
    val change = getChange
    change(0,0, "[{}]")
    change(2,3, "")
    val result3 = change(2,2, "}")
    assertResult(List(List.empty))(result3.resultOption.get)
  }

  test("regression2") {
    val change = getChange
    change(0, 0, "[]")
    change(1, 1, "{")
    change(2, 2, "}")
    change(2, 3, "")
    val result = change(2,2, "}")
    assertResult(List(List.empty))(result.resultOption.get)
  }

  def getChange: Change = {
    val text = new ParseText()
    val parser = jsonParser.getSingleResultParser(text)
    new Change {

      override def apply(from: Int, until: Int, newText: String) = {
        text.applyRangeChange(newText = newText, start = from, end = until)
        parser.changeRange(from, until, newText.length)
        parser.parse()
      }

      override def setText(newText: String): Unit = {
        text.arrayOfChars = newText.toCharArray
      }
    }
  }

  trait Change {
    def apply(from: Int, until: Int, newText: String): SingleParseResult[Any, Input]
    def setText(newText: String): Unit
  }

  // TODO add tests with linebreaks.

  // TODO add test that checks whether positions in the AST have been updated.

  // TODO add test with indentation sensitive parsing.

  def getParser = {
    jsonParser.getSingleResultParser(new ParseText())
  }

}
