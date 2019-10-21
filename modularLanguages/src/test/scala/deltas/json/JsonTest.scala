package deltas.json

import core.bigrammar.TestLanguageGrammarUtils
import core.deltas.path.{NodePath, PathRoot}
import core.language.node.Node
import core.parsers.editorParsers.{Position, UntilBestAndXStepsStopFunction}
import deltas.expression.ExpressionDelta
import deltas.json.JsonObjectLiteralDelta.{MemberValue, ObjectLiteral}
import org.scalatest.FunSuite
import util.TestLanguageBuilder

class JsonTest extends FunSuite {
  val language = TestLanguageBuilder.buildWithParser(JsonLanguage.deltas, UntilBestAndXStepsStopFunction())

  test("removes incorrect b at start") {
    val input = """b{"hello":"jo"}"""
    val compilation = language.compileString(input)
    val expectedProgram = JsonObjectLiteralDelta.neww(Map("hello" -> JsonStringLiteralDelta.neww("jo")))
    assertResult(expectedProgram)(compilation.program.asInstanceOf[PathRoot].current)
    assert(compilation.diagnostics.size == 1)
  }

  test("removes incorrect garbage at start") {
    val input = """bdddwd{"hello":"jo"}"""
    val compilation = language.compileString(input)
    val expectedProgram = JsonObjectLiteralDelta.neww(Map("hello" -> JsonStringLiteralDelta.neww("jo")))
    assertResult(expectedProgram)(compilation.program.asInstanceOf[PathRoot].current)
    assert(compilation.diagnostics.size == 1)
  }

  test("removes incorrect garbage at start 2") {
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

  test("garbage before key") {
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
    assertResult(Position(1, 7))(member.getField(MemberValue).range.get.start)
    assertResult(Position(1, 20))(member.getField(MemberValue).range.get.end)

    val stringValue = member.value
    val stringLocation = stringValue.getField(JsonStringLiteralDelta.Value)
    assertResult(Position(1, 8))(stringLocation.range.get.start)
    assertResult(Position(1, 19))(stringLocation.range.get.end)
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
