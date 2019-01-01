package deltas.javac.methods

import core.language.Language
import deltas.expression.ExpressionDelta
import deltas.javac.methods.call.{CallDelta, CallStaticOrInstanceDelta}
import org.scalatest.FunSuite

class TestCallDelta extends FunSuite {

  test("basic") {
    val language = new Language()
    ExpressionDelta.inject(language)
    MemberSelectorDelta.inject(language)
    CallStaticOrInstanceDelta.inject(language)
    assert(ExpressionDelta.expressionInstances.get(language).get(CallDelta.Shape).nonEmpty)
  }
}
