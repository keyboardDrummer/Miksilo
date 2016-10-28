package transformations.javac.methods

import core.particles.CompilationState
import org.junit.{Assert, Test}
import transformations.javac.expressions.ExpressionSkeleton
import transformations.javac.methods.call.{CallStaticOrInstanceC, CallC}

class TestCallC {


  def test() {
    val state = new CompilationState()
    ExpressionSkeleton.inject(state)
    MemberSelector.inject(state)
    CallStaticOrInstanceC.inject(state)
    assert(ExpressionSkeleton.getToInstructionsRegistry(state).get(CallC.CallKey).nonEmpty)
  }
}
