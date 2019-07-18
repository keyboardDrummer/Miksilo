package core.deltas.json

import deltas.expression.StringLiteralDelta
import deltas.json.{JsonLanguage, JsonObjectLiteralDelta}
import org.scalatest.FunSuite

class JsonTest extends FunSuite {
  val language = JsonLanguage.language

  test("removes incorrect b at start") {
    val input = """b{"hello":"jo"}"""
    val compilation = language.compileString(input)
    val expectedProgram = JsonObjectLiteralDelta.neww(Map("hello" -> StringLiteralDelta.literal("jo")))
    assertResult(expectedProgram)(compilation.program)
    assert(compilation.diagnostics.size == 1)
  }
}
