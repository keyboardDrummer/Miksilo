package transformations.types

import core.grammar.TestGrammarUtils
import org.junit.{Assert, Test}
import transformations.javac.classes.QualifiedClassName
import transformations.javac.types._

class TestParseTypes {

  @Test
  def testArrayArrayType() {
    val input = "int[][]"
    val parser = TestGrammarUtils.getJavaParser(TypeC.TypeGrammar)
    val result = parser(input).get
    Assert.assertEquals(ArrayTypeC.arrayType(ArrayTypeC.arrayType(IntTypeC.intType)), result)
  }

  @Test
  def testVoidType() {
    val input = "void"
    val parser = TestGrammarUtils.getJavaParser(TypeC.TypeGrammar)
    val result = parser(input).get
    Assert.assertEquals(VoidTypeC.voidType, result)
  }

  @Test
  def testArrayType() {
    val input = "int[]"
    val parser = TestGrammarUtils.getJavaParser(TypeC.TypeGrammar)
    val result = parser(input).get
    Assert.assertEquals(ArrayTypeC.arrayType(IntTypeC.intType), result)
  }

  @Test
  def testObjectType() {
    val input = "java.lang.String"
    val parser = TestGrammarUtils.getJavaParser(TypeC.TypeGrammar)
    val parseResult = parser(input)
    if (parseResult.isEmpty)
      Assert.fail(parseResult.toString)

    val result = parseResult.get
    val objectType = ObjectTypeC.objectType(new QualifiedClassName(Seq("java", "lang", "String")))
    Assert.assertEquals(objectType, result)
  }

  @Test
  def testArrayType2() {
    val input = "java.lang.String[]"
    val parser = TestGrammarUtils.getJavaParser(TypeC.TypeGrammar)
    val parseResult = parser(input)
    if (parseResult.isEmpty)
      Assert.fail(parseResult.toString)

    val result = parseResult.get
    val objectType = ObjectTypeC.objectType(new QualifiedClassName(Seq("java", "lang", "String")))
    Assert.assertEquals(ArrayTypeC.arrayType(objectType), result)
  }
}
