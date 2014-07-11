package core.transformation

import org.junit.{Assert, Test}
import transformations.javac.base.model.JavaBaseModel
import transformations.javac.expressions.LiteralC

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
    val first = JavaBaseModel.call(JavaBaseModel.selector(JavaBaseModel.selector(JavaBaseModel.variable("System"), "out"), "print"),
      List(JavaBaseModel.call(JavaBaseModel.variable("fibonacci"), List(LiteralC.literal(5)))))
    val second = JavaBaseModel.call(JavaBaseModel.selector(JavaBaseModel.selector(JavaBaseModel.variable("System"), "out"), "print"),
      List(JavaBaseModel.call(JavaBaseModel.variable("fibonacci"), List(LiteralC.literal(5)))))
    Assert.assertEquals(first, second)
  }


}
