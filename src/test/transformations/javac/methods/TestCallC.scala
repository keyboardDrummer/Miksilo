package transformations.javac.methods

import core.particles.CompilationState
import org.junit.{Assert, Test}
import transformations.javac.expressions.ExpressionSkeleton

class TestCallC {

  @Test
  def test() {
    val state = new CompilationState()
    ExpressionSkeleton.inject(state)
    MemberSelector.inject(state)
    CallC.inject(state)
    Assert.assertTrue(ExpressionSkeleton.getToInstructionsRegistry(state).get(CallC.CallKey).nonEmpty)
  }
}
