package transformations.javac.methods

import core.particles.CompilationState
import org.junit.{Assert, Test}
import org.scalatest.FunSuite
import transformations.javac.expressions.ExpressionSkeleton
import transformations.javac.methods.call.{CallC, CallStaticOrInstanceC}

class TestCallC extends FunSuite {

  test("callC") {
    val state = new CompilationState()
    ExpressionSkeleton.inject(state)
    MemberSelector.inject(state)
    CallStaticOrInstanceC.inject(state)
    assert(ExpressionSkeleton.getToInstructionsRegistry(state).get(CallC.CallKey).nonEmpty)
  }
}
