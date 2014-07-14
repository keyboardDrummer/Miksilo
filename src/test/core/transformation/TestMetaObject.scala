package core.transformation

import org.junit.{Assert, Test}
import transformations.javac.expressions.LiteralC
import transformations.javac.methods.{CallC, SelectorC, VariableC}

class TestMetaObject {

  object ClazzKey

  object FieldKey

  object FieldValue

  @Test
  def testEquals() {
    val first = new MetaObject(ClazzKey) {
      data.put(FieldKey, FieldValue)
    }
    val second = new MetaObject(ClazzKey) {
      data.put(FieldKey, FieldValue)
    }
    Assert.assertEquals(first, second)
  }

  @Test
  def testEqualsOnJavaModel() {
    val first = CallC.call(SelectorC.selector(SelectorC.selector(VariableC.variable("System"), "out"), "print"),
      List(CallC.call(VariableC.variable("fibonacci"), List(LiteralC.literal(5)))))
    val second = CallC.call(SelectorC.selector(SelectorC.selector(VariableC.variable("System"), "out"), "print"),
      List(CallC.call(VariableC.variable("fibonacci"), List(LiteralC.literal(5)))))
    Assert.assertEquals(first, second)
  }


}
