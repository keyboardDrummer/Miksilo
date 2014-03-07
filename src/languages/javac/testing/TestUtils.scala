package languages.javac.testing

import transformation.{ComparisonOptions, MetaObject}
import languages.bytecode.ByteCode
import org.junit.Assert

object TestUtils {


  def testMethodEquivalence(expectedByteCode: MetaObject, compiledCode: MetaObject) {
    for(methodPair <- ByteCode.getMethods(expectedByteCode).zip(ByteCode.getMethods(compiledCode)))
    {
      Assert.assertTrue(MetaObject.deepEquality(methodPair._1, methodPair._2,
        new ComparisonOptions(false, true, false)))
    }
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
