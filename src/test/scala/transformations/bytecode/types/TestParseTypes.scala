package transformations.bytecode.types

import core.bigrammar.TestGrammarUtils
import org.junit.{Assert, Test}
import org.scalatest.FunSuite
import transformations.javac.classes.skeleton.QualifiedClassName

class TestParseTypes extends FunSuite {

  test("ArrayArrayType") {
    val input = "int[][]"
    val result = TestGrammarUtils.getGrammarResult(input, TypeSkeleton.JavaTypeGrammar)
    assertResult(ArrayTypeC.arrayType(ArrayTypeC.arrayType(IntTypeC.intType)))(result)
  }

  test("VoidType") {
    val input = "void"
    val result = TestGrammarUtils.getGrammarResult(input, TypeSkeleton.JavaTypeGrammar)
    assertResult(VoidTypeC.voidType)(result)
  }

  test("ArrayType") {
    val input = "int[]"
    val result = TestGrammarUtils.getGrammarResult(input, TypeSkeleton.JavaTypeGrammar)
    assertResult(ArrayTypeC.arrayType(IntTypeC.intType))(result)
  }

  test("ObjectType") {
    val input = "java.lang.String"
    val result = TestGrammarUtils.getGrammarResult(input, TypeSkeleton.JavaTypeGrammar)
    val objectType = ObjectTypeC.objectType(new QualifiedClassName(Seq("java", "lang", "String")))
    assertResult(objectType)(result)
  }

  test("ArrayType2") {
    val input = "java.lang.String[]"
    val result = TestGrammarUtils.getGrammarResult(input, TypeSkeleton.JavaTypeGrammar)
    val objectType = ObjectTypeC.objectType(new QualifiedClassName(Seq("java", "lang", "String")))
    assertResult(ArrayTypeC.arrayType(objectType))(result)
  }
}
