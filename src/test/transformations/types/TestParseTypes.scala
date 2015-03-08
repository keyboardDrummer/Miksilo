package transformations.types

import core.biGrammar.TestGrammarUtils
import org.junit.{Assert, Test}
import transformations.javac.classes.QualifiedClassName

class TestParseTypes {

  @Test
  def testArrayArrayType() {
    val input = "int[][]"
    val result = TestGrammarUtils.getGrammarResult(input, TypeSkeleton.TypeGrammar)
    Assert.assertEquals(ArrayTypeC.arrayType(ArrayTypeC.arrayType(IntTypeC.intType)), result)
  }

  @Test
  def testVoidType() {
    val input = "void"
    val result = TestGrammarUtils.getGrammarResult(input, TypeSkeleton.TypeGrammar)
    Assert.assertEquals(VoidTypeC.voidType, result)
  }

  @Test
  def testArrayType() {
    val input = "int[]"
    val result = TestGrammarUtils.getGrammarResult(input, TypeSkeleton.TypeGrammar)
    Assert.assertEquals(ArrayTypeC.arrayType(IntTypeC.intType), result)
  }

  @Test
  def testObjectType() {
    val input = "java.lang.String"
    val result = TestGrammarUtils.getGrammarResult(input, TypeSkeleton.TypeGrammar)
    val objectType = ObjectTypeC.objectType(new QualifiedClassName(Seq("java", "lang", "String")))
    Assert.assertEquals(objectType, result)
  }

  @Test
  def testArrayType2() {
    val input = "java.lang.String[]"
    val result = TestGrammarUtils.getGrammarResult(input, TypeSkeleton.TypeGrammar)
    val objectType = ObjectTypeC.objectType(new QualifiedClassName(Seq("java", "lang", "String")))
    Assert.assertEquals(ArrayTypeC.arrayType(objectType), result)
  }
}
