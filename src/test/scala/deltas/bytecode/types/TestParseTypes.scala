package deltas.bytecode.types

import core.bigrammar.TestCompilerGrammarUtils
import core.deltas.Delta
import core.deltas.node.Node
import org.scalatest.FunSuite
import deltas.bytecode.attributes.CodeAttribute.CodeKey
import deltas.bytecode.attributes.{CodeAttribute, StackMapTableAttribute}
import deltas.bytecode.simpleBytecode.{LabelDelta, LabelledLocations}
import deltas.javac.JavaCompilerDeltas
import deltas.javac.classes.skeleton.QualifiedClassName

class TestParseTypes extends FunSuite {

  test("ArrayArrayType") {
    val input = "int[][]"
    val result = TestCompilerGrammarUtils.getGrammarResult(input, TypeSkeleton.JavaTypeGrammar)
    assertResult(ArrayTypeC.arrayType(ArrayTypeC.arrayType(IntTypeC.intType)))(result)
  }

  test("VoidType") {
    val input = "void"
    val result = TestCompilerGrammarUtils.getGrammarResult(input, TypeSkeleton.JavaTypeGrammar)
    assertResult(VoidTypeC.voidType)(result)
  }

  test("appendFrame") {
    val input = "append frame int int int"
    val result = TestCompilerGrammarUtils.getGrammarResult(input, StackMapTableAttribute.StackMapFrameGrammar)
    assertResult(StackMapTableAttribute.AppendFrame)(result.asInstanceOf[Node].clazz)
  }

  test("labelWithAppendFrame") {
    val input = "label \"start-4962768465676381896\"\n        append frame int int"
    val result = TestCompilerGrammarUtils(Seq[Delta](LabelledLocations) ++ JavaCompilerDeltas.byteCodeDeltas).
      getGrammarResult(input, CodeAttribute.InstructionGrammar)
    assertResult(LabelDelta.LabelKey)(result.asInstanceOf[Node].clazz)
  }

  test("labelWithAppendFrameInInstructions1") {
    val input = "Code: name:9, stack:2, locals:3\n    \n " +
      "label \"start-4962768465676381896\"\n        same frame\n iload 2 \n    Exceptions:"
    val result = TestCompilerGrammarUtils(Seq[Delta](LabelledLocations) ++ JavaCompilerDeltas.byteCodeDeltas).
      getGrammarResult(input, CodeAttribute.CodeKey)
    assertResult(CodeKey)(result.asInstanceOf[Node].clazz)
  }

  ignore("labelWithAppendFrameInInstructions2") {
    val input = "code: name:9, stack:2, locals:3\n    \n " +
      "label \"start-4962768465676381896\"\n        append frame int int\n iload 2 \n    Exceptions:"
    val result = TestCompilerGrammarUtils(Seq[Delta](LabelledLocations) ++ JavaCompilerDeltas.byteCodeDeltas).
      getGrammarResult(input, CodeAttribute.CodeKey)
    assertResult(CodeKey)(result.asInstanceOf[Node].clazz)
  }

  test("intType") {
    val input = "int"
    val result = TestCompilerGrammarUtils.getGrammarResult(input, TypeSkeleton.JavaTypeGrammar)
    assertResult(IntTypeC.intType)(result)
  }

  test("ArrayType") {
    val input = "int[]"
    val result = TestCompilerGrammarUtils.getGrammarResult(input, TypeSkeleton.JavaTypeGrammar)
    assertResult(ArrayTypeC.arrayType(IntTypeC.intType))(result)
  }

  test("ObjectType") {
    val input = "java.lang.String"
    val result = TestCompilerGrammarUtils.getGrammarResult(input, TypeSkeleton.JavaTypeGrammar)
    val objectType = ObjectTypeDelta.objectType(new QualifiedClassName(Seq("java", "lang", "String")))
    assertResult(objectType)(result)
  }

  test("ArrayType2") {
    val input = "java.lang.String[]"
    val result = TestCompilerGrammarUtils.getGrammarResult(input, TypeSkeleton.JavaTypeGrammar)
    val objectType = ObjectTypeDelta.objectType(new QualifiedClassName(Seq("java", "lang", "String")))
    assertResult(ArrayTypeC.arrayType(objectType))(result)
  }
}
