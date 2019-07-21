package core.deltas.json

import deltas.expression.ExpressionDelta
import deltas.json.{JsonLanguage, JsonObjectLiteralDelta, JsonStringLiteralDelta}
import org.scalatest.FunSuite

class JsonTest extends FunSuite {
  val language = JsonLanguage.language

  test("removes incorrect b at start") {
    val input = """b{"hello":"jo"}"""
    val compilation = language.compileString(input)
    val expectedProgram = JsonObjectLiteralDelta.neww(Map("hello" -> JsonStringLiteralDelta.neww("jo")))
    assertResult(expectedProgram)(compilation.program)
    assert(compilation.diagnostics.size == 1)
  }

  test("removes incorrect garbage at start") {
    val input = """bdddwd{"hello":"jo"}"""
    val compilation = language.compileString(input)
    val expectedProgram = JsonObjectLiteralDelta.neww(Map("hello" -> JsonStringLiteralDelta.neww("jo")))
    assertResult(expectedProgram)(compilation.program)
    assert(compilation.diagnostics.size == 1)
  }

  test("removes incorrect garbage at start 2") {
    val input = """uuygyuiijuihh{"hello"}"""
    val compilation = language.compileString(input)
    val expectedProgram = JsonObjectLiteralDelta.neww(Map("hello" -> ExpressionDelta.value))
    assertResult(expectedProgram)(compilation.program)
    assert(compilation.diagnostics.size == 2)
  }

  test("removes incorrect garbage") {
    val input = """iuhiuihu"""
    val compilation = language.compileString(input)
    val expectedProgram = ExpressionDelta.value
    assertResult(expectedProgram)(compilation.program)
    assert(compilation.diagnostics.size == 2)
  }

  test("removes incorrect garbage 2") {
    val input = """w"""
    val compilation = language.compileString(input)
    val expectedProgram = ExpressionDelta.value
    assertResult(expectedProgram)(compilation.program)
    assert(compilation.diagnostics.size == 2)
  }

  test("garbage before key") {
    val input = """{f"hello"}"""
    val compilation = language.compileString(input)
    val expectedProgram = JsonObjectLiteralDelta.neww(Map("hello" -> ExpressionDelta.value))
    assertResult(expectedProgram)(compilation.program)
    assert(compilation.diagnostics.size == 2)
  }
}
