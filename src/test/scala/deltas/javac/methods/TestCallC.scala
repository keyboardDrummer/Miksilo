package deltas.javac.methods

import core.deltas.Language
import org.junit.{Assert, Test}
import org.scalatest.FunSuite
import deltas.javac.expressions.ExpressionSkeleton
import deltas.javac.methods.call.{CallC, CallStaticOrInstanceC}

class TestCallC extends FunSuite {

  test("callC") {
    val state = new Language()
    ExpressionSkeleton.inject(state)
    MemberSelector.inject(state)
    CallStaticOrInstanceC.inject(state)
    assert(ExpressionSkeleton.getToInstructionsRegistry(state).get(CallC.CallKey).nonEmpty)
  }
}
