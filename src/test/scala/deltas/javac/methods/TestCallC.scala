package deltas.javac.methods

import core.language.Language
import deltas.javac.expressions.ExpressionSkeleton
import deltas.javac.methods.call.{CallDelta, CallStaticOrInstanceDelta}
import org.scalatest.FunSuite

class TestCallC extends FunSuite {

  test("callC") {
    val state = new Language(Seq.empty)
    ExpressionSkeleton.inject(state)
    MemberSelector.inject(state)
    CallStaticOrInstanceDelta.inject(state)
    assert(ExpressionSkeleton.getToInstructionsRegistry(state).get(CallDelta.CallKey).nonEmpty)
  }
}
