package transformations.javac.methods

import core.transformation.TransformationState
import org.junit.{Assert, Test}
import transformations.javac.expressions.ExpressionSkeleton

class TestCallC {

  @Test
  def test() {
    val state = new TransformationState()
    ExpressionSkeleton.inject(state)
    CallC.inject(state)
    Assert.assertTrue(ExpressionSkeleton.getExpressionToLines(state).get(CallC.CallKey).nonEmpty)
  }
}
