package transformations.javac.methods

import core.transformation.TransformationState
import org.junit.{Assert, Test}
import transformations.javac.expressions.ExpressionC

class TestCallC {

  @Test
  def test() {
    val state = new TransformationState()
    CallC.inject(state)
    Assert.assertTrue(ExpressionC.getExpressionToLines(state).get(CallC.CallKey).nonEmpty)
  }
}
