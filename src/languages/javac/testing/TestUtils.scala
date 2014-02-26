package languages.javac.testing

import transformation.{ComparisonOptions, MetaObject}
import languages.bytecode.ByteCode
import org.junit.Assert

object TestUtils {


  def testMethodEquivalence(expectedByteCode: MetaObject, compiledCode: MetaObject) {
    val expectedMethod = ByteCode.getMethods(expectedByteCode)(0)
    val compiledMethod = ByteCode.getMethods(compiledCode)(0)
    Assert.assertTrue(MetaObject.deepEquality(compiledMethod, expectedMethod,
      new ComparisonOptions(false, false, true)))
  }


  def compareConstantPools(expectedByteCode: MetaObject, compiledCode: MetaObject) {
    val expectedConstantPoolSet = ByteCode.getConstantPool(expectedByteCode)
    val compiledConstantPoolSet = ByteCode.getConstantPool(compiledCode)
    Assert.assertEquals(expectedConstantPoolSet.length, compiledConstantPoolSet.length)
    Assert.assertTrue(expectedConstantPoolSet.forall(expectedItem => {
      val hasEquivalent = compiledConstantPoolSet.exists(compiledItem => MetaObject.deepEquality(compiledItem, expectedItem,
        new ComparisonOptions(false, false, true)))
      hasEquivalent
    }))
  }
}
