package deltas.javac.methods

import core.language.Language
import deltas.expressions.ExpressionDelta
import deltas.javac.expressions.ByteCodeExpressionSkeleton
import deltas.javac.methods.call.{CallDelta, CallStaticOrInstanceDelta}
import org.scalatest.FunSuite

class TestCallDelta extends FunSuite {

  test("basic") {
    val language = new Language()
    ExpressionDelta.inject(language)
    MemberSelectorDelta.inject(language)
    CallStaticOrInstanceDelta.inject(language)
    assert(ByteCodeExpressionSkeleton.expressionInstances.get(language).get(CallDelta.Shape).nonEmpty)
  }
}
