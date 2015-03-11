package core.particles

import org.junit.{Assert, Test}
import transformations.javac.expressions.literals.IntLiteralC
import transformations.javac.methods.{CallC, MemberSelector, VariableC}

class TestMetaObject {

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
    val first = CallC.call(MemberSelector.selector(MemberSelector.selector(VariableC.variable("System"), "out"), "print"),
      List(CallC.call(VariableC.variable("fibonacci"), List(IntLiteralC.literal(5)))))
    val second = CallC.call(MemberSelector.selector(MemberSelector.selector(VariableC.variable("System"), "out"), "print"),
      List(CallC.call(VariableC.variable("fibonacci"), List(IntLiteralC.literal(5)))))
    Assert.assertEquals(first, second)
  }

  object ClazzKey

  object FieldKey

  object FieldValue


}
