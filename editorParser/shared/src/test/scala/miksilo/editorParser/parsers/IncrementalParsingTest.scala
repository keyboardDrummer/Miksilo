//package miksilo.editorParser.parsers
//
//import miksilo.editorParser.languages.json.JsonParser
//import miksilo.editorParser.parsers.caching.ArrayOffsetManager
//import miksilo.editorParser.parsers.core.ParseText
//import miksilo.editorParser.parsers.editorParsers.SingleParseResult
//import org.scalatest.funsuite.AnyFunSuite
//
//import scala.collection.immutable.ListMap
//
//class IncrementalParsingTest extends AnyFunSuite {
//
//  test("inserting between two empty braces works") {
//    val input = """{}"""
//    val change = getChange
//    val result1 = change(0,0, input)
//    val result3 = change(1,1, """"hello":"bla"""")
//    assertResult(ListMap("hello" -> "bla"))(result3.resultOption.get)
//  }
//
//  test("removing works") {
//    val input = """{"hello":"bla"}"""
//    val input2 = """{}"""
//    val change = getChange
//    change(0,0, input)
//    val result3 = change(1,14, "")
//    assertResult(ListMap())(result3.resultOption.get)
//  }
//
//  test("partial clearing works") {
//    val input = """{"foo":"bar"}"""
//    val input2 = """{"bar":"foo"}"""
//    val change = getChange
//    change(0,0, input)
//    change.setText(input2)
//    val result = change(2,5, "bar")
//    assertResult(ListMap("bar" -> "bar"))(result.resultOption.get)
//  }
//
//  test("inserts work") {
//    val input = """[1,3]"""
//    val input2 = """[1,2,0]"""
//    val change = getChange
//    change(0,0,input)
//    change.setText(input2)
//    val result = change(2,2, ",2")
//    assertResult(Array(1,2,3))(result.resultOption.get)
//  }
//
//  test("multiple inserts work") {
//    val input = """[1,3,5]"""
//    val input2 = """[1,2,0,0]"""
//    val input3 = """[0,2,3,4,0]"""
//    val change = getChange
//    change(0,0,"[1,3,5]")
//    change.setText(input2)
//    change(2,2,",2")
//    change.setText(input3)
//    val result = change(6,6,",4")
//    assertResult(Array(1,2,3,4,5))(result.resultOption.get)
//  }
//
//  test("inserting at the end workds") {
//    val change = getChange
//    change(0,0,"1")
//    val result = change(1,1,"35")
//    assertResult(135)(result.resultOption.get)
//  }
//
//  test("success parser caching") {
//    val change = getChange
//    change(0,0, "[]")
//    change(1,1, "{")
//    val result = change(2,2, "}")
//    assertResult(Array(ListMap.empty))(result.resultOption.get)
//  }
//
//  test("regression") {
//    val change = getChange
//    change(0,0, "[{}]")
//    change(2,3, "")
//    val result3 = change(2,2, "}")
//    assertResult(List(ListMap.empty))(result3.resultOption.get)
//  }
//
//  test("regression2") {
//    val change = getChange
//    change(0, 0, "[]")
//    change(1, 1, "{")
//    change(2, 2, "}")
//    change(2, 3, "")
//    val result = change(2,2, "}")
//    assertResult(List(ListMap.empty))(result.resultOption.get)
//  }
//
//  test("regression3") {
//    val change = getChange
//    val result0 = change(0, 0, """{ "bla": { }}""")
//    assert(result0.errors.isEmpty)
//    val result1 = change(11, 13, "")
//    assert(result1.errors.nonEmpty)
//    val result2 = change(11, 11, "}")
//    assert(result2.errors.nonEmpty)
//    val result3 = change(12, 12, "}")
//    assert(result3.errors.isEmpty)
//  }
//
//  test("regression4") {
//    val program = """{
//                    |  "a" : {
//                    |    "b" : "c",
//                    |    "d" : {
//                    |      "e" : "f"
//                    |    }
//                    |  }
//                    |}""".stripMargin
//
//    val insert = """
//                   |    "b" : "c",
//                   |    "d" : {
//                   |      "e" : "f"
//                   |    }""".stripMargin
//
//    val change = getChange
//    val result0 = change(0, 0, program)
//    val result1 = change(11, 60, "")
//    val result2 = change(11, 11, insert)
//    assert(result1.errors.isEmpty)
//  }
//
//  def getChange: Change = {
//    val text = new ParseText()
//    val parser = ArrayOffsetManager.getCachingParser(text, JsonParser.parser, indentationSensitive = false)
//    new Change {
//
//      override def apply(from: Int, until: Int, newText: String) = {
//        text.applyRangeChange(start = from, end = until, newText = newText)
//        parser.changeRange(from, until, newText.length)
//        val parseResult = parser.parse()
//        parseResult.map(JsonTestUtils.valueToPrimitive)
//      }
//
//      override def setText(newText: String): Unit = {
//        text.arrayOfChars = newText.toCharArray
//      }
//    }
//  }
//
//  trait Change {
//    def apply(from: Int, until: Int, newText: String): SingleParseResult[Any]
//    def setText(newText: String): Unit
//  }
//
//  // TODO add tests with linebreaks.
//
//  // TODO add test that checks whether positions in the AST have been updated.
//
//  // TODO add test with indentation sensitive parsing.
//
//}
