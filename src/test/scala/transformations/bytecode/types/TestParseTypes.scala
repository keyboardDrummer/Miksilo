package transformations.bytecode.types

import core.bigrammar.TestGrammarUtils
import core.particles.Delta
import core.particles.node.Node
import org.scalatest.FunSuite
import transformations.bytecode.additions.LabelledLocations
import transformations.bytecode.additions.LabelledLocations.LabelKey
import transformations.bytecode.attributes.CodeAttribute.CodeKey
import transformations.bytecode.attributes.{CodeAttribute, StackMapTableAttribute}
import transformations.javac.JavaCompiler
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

  test("appendFrame") {
    val input = "append frame int int int"
    val result = TestGrammarUtils.getGrammarResult(input, StackMapTableAttribute.StackMapFrameGrammar)
    assertResult(StackMapTableAttribute.AppendFrame)(result.asInstanceOf[Node].clazz)
  }

  test("labelWithAppendFrame") {
    val input = "label(\"start-4962768465676381896\")\n        append frame int int"
    val result = TestGrammarUtils(Seq[Delta](LabelledLocations) ++ JavaCompiler.byteCodeTransformations).
      getGrammarResult(input, CodeAttribute.InstructionGrammar)
    assertResult(LabelKey)(result.asInstanceOf[Node].clazz)
  }

  test("labelWithAppendFrameInInstructions1") {
    val input = "code: nameIndex:9, maxStack:2, maxLocal:3\n    instructions:\n " +
      "label(\"start-4962768465676381896\")\n        same frame\n load integer(2) \n    attributes:\n    exceptions:"
    val result = TestGrammarUtils(Seq[Delta](LabelledLocations) ++ JavaCompiler.byteCodeTransformations).
      getGrammarResult(input, CodeAttribute.CodeGrammar)
    assertResult(CodeKey)(result.asInstanceOf[Node].clazz)
  }

  ignore("labelWithAppendFrameInInstructions2") {
    val input = "code: nameIndex:9, maxStack:2, maxLocal:3\n    instructions:\n " +
      "label(\"start-4962768465676381896\")\n        append frame int int\n load integer(2) \n    attributes:\n    exceptions:"
    val result = TestGrammarUtils(Seq[Delta](LabelledLocations) ++ JavaCompiler.byteCodeTransformations).
      getGrammarResult(input, CodeAttribute.CodeGrammar)
    assertResult(CodeKey)(result.asInstanceOf[Node].clazz)
  }

  test("intType") {
    val input = "int"
    val result = TestGrammarUtils.getGrammarResult(input, TypeSkeleton.JavaTypeGrammar)
    assertResult(IntTypeC.intType)(result)
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
