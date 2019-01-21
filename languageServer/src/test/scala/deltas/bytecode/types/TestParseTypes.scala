package deltas.bytecode.types

import core.bigrammar.TestLanguageGrammarUtils
import core.deltas.Delta
import core.language.node.Node
import org.scalatest.FunSuite
import deltas.bytecode.attributes.CodeAttributeDelta.CodeKey
import deltas.bytecode.attributes.{CodeAttributeDelta, StackMapTableAttributeDelta}
import deltas.bytecode.simpleBytecode.{LabelDelta, LabelledLocations}
import deltas.javac.JavaLanguage
import deltas.javac.classes.skeleton.QualifiedClassName

class TestParseTypes extends FunSuite {

  test("ArrayArrayType") {
    val input = "int[][]"
    val result = TestLanguageGrammarUtils.parse(input, TypeSkeleton.JavaTypeGrammar)
    assertResult(ArrayTypeDelta.arrayType(ArrayTypeDelta.arrayType(IntTypeDelta.intType)))(result)
  }

  test("VoidType") {
    val input = "void"
    val result = TestLanguageGrammarUtils.parse(input, TypeSkeleton.JavaTypeGrammar)
    assertResult(VoidTypeDelta.voidType)(result)
  }

  test("appendFrame") {
    val input = "appendFrame int int int"
    val result = TestLanguageGrammarUtils.parse(input, StackMapTableAttributeDelta.StackMapFrameGrammar)
    assertResult(StackMapTableAttributeDelta.AppendFrame)(result.asInstanceOf[Node].shape)
  }

  test("labelWithAppendFrame") {
    val input = "label start-4962768465676381896\n        appendFrame int int"
    val result = TestLanguageGrammarUtils(Seq[Delta](LabelledLocations) ++ JavaLanguage.byteCodeDeltas).
      parse(input, CodeAttributeDelta.InstructionGrammar)
    assertResult(LabelDelta.Shape)(result.asInstanceOf[Node].shape)
  }

  test("labelWithAppendFrameInInstructions1") {
    val input = "Code: name:9, stack:2, locals:3\n    \n " +
      "label start-4962768465676381896\n        sameFrame\n iload 2 \n    Exceptions:"
    val result = TestLanguageGrammarUtils(Seq[Delta](LabelledLocations) ++ JavaLanguage.byteCodeDeltas).
      parse(input, CodeAttributeDelta.CodeKey)
    assertResult(CodeKey)(result.asInstanceOf[Node].shape)
  }

  ignore("labelWithAppendFrameInInstructions2") {
    val input = "code: name:9, stack:2, locals:3\n    \n " +
      "label \"start-4962768465676381896\"\n        appendFrame int int\n iload 2 \n    Exceptions:"
    val result = TestLanguageGrammarUtils(Seq[Delta](LabelledLocations) ++ JavaLanguage.byteCodeDeltas).
      parse(input, CodeAttributeDelta.CodeKey)
    assertResult(CodeKey)(result.asInstanceOf[Node].shape)
  }

  test("intType") {
    val input = "int"
    val result = TestLanguageGrammarUtils.parse(input, TypeSkeleton.JavaTypeGrammar)
    assertResult(IntTypeDelta.intType)(result)
  }

  test("ArrayType") {
    val input = "int[]"
    val result = TestLanguageGrammarUtils.parse(input, TypeSkeleton.JavaTypeGrammar)
    assertResult(ArrayTypeDelta.arrayType(IntTypeDelta.intType))(result)
  }

  test("ObjectType") {
    val input = "java.lang.String"
    val result = TestLanguageGrammarUtils.parse(input, TypeSkeleton.JavaTypeGrammar)
    val objectType = QualifiedObjectTypeDelta.neww(new QualifiedClassName(Seq("java", "lang", "String")))
    assertResult(objectType)(result)
  }

  test("ArrayType2") {
    val input = "java.lang.String[]"
    val result = TestLanguageGrammarUtils.parse(input, TypeSkeleton.JavaTypeGrammar)
    val objectType = QualifiedObjectTypeDelta.neww(new QualifiedClassName(Seq("java", "lang", "String")))
    assertResult(ArrayTypeDelta.arrayType(objectType))(result)
  }
}
