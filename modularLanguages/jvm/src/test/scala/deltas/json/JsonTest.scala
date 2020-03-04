package deltas.json

import core.bigrammar.TestLanguageGrammarUtils
import core.deltas.path.{NodePath, PathRoot}
import core.language.node.Node
import core.parsers.core.ParseText
import core.parsers.editorParsers.{Position, UntilBestAndXStepsStopFunction}
import deltas.expression.ExpressionDelta
import deltas.json.JsonObjectLiteralDelta.{MemberValue, ObjectLiteral}
import languageServer.{LanguageServerTest, MiksiloLanguageServer}
import org.scalatest.funsuite.AnyFunSuite
import util.TestLanguageBuilder

class JsonTest extends AnyFunSuite {
  val language = TestLanguageBuilder.buildWithParser(JsonLanguage.deltas, UntilBestAndXStepsStopFunction())

  ignore("removes incorrect b at start") {
    val input = """b{"hello":"jo"}"""
    val compilation = language.compileString(input)
    val expectedProgram = JsonObjectLiteralDelta.neww(Map("hello" -> JsonStringLiteralDelta.neww("jo")))
    assertResult(expectedProgram)(compilation.program.asInstanceOf[PathRoot].current)
    assert(compilation.diagnostics.size == 1)
  }

  ignore("removes incorrect garbage at start") {
    val input = """bdddwd{"hello":"jo"}"""
    val compilation = language.compileString(input)
    val expectedProgram = JsonObjectLiteralDelta.neww(Map("hello" -> JsonStringLiteralDelta.neww("jo")))
    assertResult(expectedProgram)(compilation.program.asInstanceOf[PathRoot].current)
    assert(compilation.diagnostics.size == 1)
  }

  ignore("removes incorrect garbage at start 2") {
    val input = """uuygyuiijuihh{"hello"}"""
    val compilation = language.compileString(input)
    val expectedProgram = JsonObjectLiteralDelta.neww(Map("hello" -> ExpressionDelta.value))
    assertResult(expectedProgram)(compilation.program.asInstanceOf[PathRoot].current)
    assert(compilation.diagnostics.size == 2)
  }

  test("removes incorrect garbage") {
    val input = """iuhiuihu"""
    val compilation = language.compileString(input)
    val expectedProgram = ExpressionDelta.value
    assertResult(expectedProgram)(compilation.program.asInstanceOf[PathRoot].current)
    assert(compilation.diagnostics.size == 2)
  }

  test("removes incorrect garbage 2") {
    val input = """w"""
    val compilation = language.compileString(input)
    val expectedProgram = ExpressionDelta.value
    assertResult(expectedProgram)(compilation.program.asInstanceOf[PathRoot].current)
    assert(compilation.diagnostics.size == 2)
  }

  ignore("garbage before key") {
    val input = """{f"hello"}"""
    val compilation = language.compileString(input)
    val expectedProgram = JsonObjectLiteralDelta.neww(Map("hello" -> ExpressionDelta.value))
    assertResult(expectedProgram)(compilation.program.asInstanceOf[PathRoot].current)
    assert(compilation.diagnostics.size == 2)
  }

  val utils = TestLanguageGrammarUtils(JsonLanguage.deltas)

  test("testStringValueSourceLocation") {
    val example =
      """{
        |  "x": "stringValue"
        |}""".stripMargin

    val result: ObjectLiteral[NodePath] = PathRoot(utils.parse(example).asInstanceOf[Node])
    val member = result.members.head
    val offsetRange = member.getField(MemberValue).range.get
    val text = new ParseText(example)
    val sourceRange = offsetRange.toRange(text)
    assertResult(Position(1, 7))(sourceRange.start)
    assertResult(Position(1, 20))(sourceRange.end)

    val stringValue = member.value
    val stringLocation = stringValue.getField(JsonStringLiteralDelta.Value).range.get.toRange(text)
    assertResult(Position(1, 8))(stringLocation.start)
    assertResult(Position(1, 19))(stringLocation.end)
  }

  test("missing object member key") {
    val program = """{:3}""".stripMargin
    val reference = """{"":3}""".stripMargin

    val result = language.compileString(program).program
    val expectedProgram = language.compileString(reference).program
    assertResult(expectedProgram)(result)
  }

  test("parseSimpleJson") {
    val example =
      """{
        |  "x": 3
        |}""".stripMargin

    utils.compareInputWithPrint(example)
  }

  test("optionalComma") {
    val example =
      """{
        |  "x": 3,
        |}""".stripMargin

    val expected =
      """{
        |  "x": 3
        |}""".stripMargin   //"x":<value> 3

    val ast = utils.parse(example)
    val printResult = utils.getPrintResult(ast)
    assertResult(expected)(printResult)
  }
}

class JsonServerTest extends AnyFunSuite with LanguageServerTest {

  test("regression2") {
    val input = """[]"""  // [1,2,3,4]     [1,2,[],3]     [{"name":"Remy", age: 31}, {"name":"Tzeni",age:29}]

    val jsonLanguage = JsonLanguage.language
    val server = new MiksiloLanguageServer(jsonLanguage)

    val document = this.openDocument(server, input)
    val results1 = applyChange(server, document, 1,1, "{") // [{]
    val results2 = applyChange(server, document, 2,2, "}") // [{}]
    assert(results2.isEmpty)
    val results3 = applyChange(server, document, 2,3, "")  // [{]
    val results4 = applyChange(server, document, 2,2, "}") // [{}]
    assert(results4.isEmpty)
  }
}
