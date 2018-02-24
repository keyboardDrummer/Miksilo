package deltas.javac.methods

import core.language.Language
import deltas.javac.expressions.ExpressionSkeleton
import deltas.javac.methods.call.{CallDelta, CallStaticOrInstanceDelta}
import org.scalatest.FunSuite

class TestCallDelta extends FunSuite {

  test("basic") {
    val language = new Language()
    ExpressionSkeleton.inject(language)
    MemberSelectorDelta.inject(language)
    CallStaticOrInstanceDelta.inject(language)
    assert(ExpressionSkeleton.expressionInstances.get(language).get(CallDelta.Shape).nonEmpty)
  }
}
