package transformations.javac.methods

import core.transformation.{MetaObject, TransformationState}
import org.junit.{Assert, Test}
import transformations.javac.expressions.ExpressionC

class TestCallC {

  @Test
  def test() {
    val state = new TransformationState()
    CallC.transform(new MetaObject(null), state)
    Assert.assertTrue(ExpressionC.getExpressionToLines(state).get(CallC.CallKey).nonEmpty)
  }
}
