package transformations.javac.methods

import core.particles.Language
import org.junit.{Assert, Test}
import org.scalatest.FunSuite
import transformations.javac.expressions.ExpressionSkeleton
import transformations.javac.methods.call.{CallC, CallStaticOrInstanceC}

class TestCallC extends FunSuite {

  test("callC") {
    val state = new Language()
    ExpressionSkeleton.inject(state)
    MemberSelector.inject(state)
    CallStaticOrInstanceC.inject(state)
    assert(ExpressionSkeleton.getToInstructionsRegistry(state).get(CallC.CallKey).nonEmpty)
  }
}
