package miksilo.modularLanguages.deltas.bytecode

import miksilo.modularLanguages.core.deltas.path.PathRoot
import miksilo.modularLanguages.core.node.Node
import miksilo.modularLanguages.deltas.bytecode.attributes.{CodeAttributeDelta, StackMapTableAttributeDelta}
import miksilo.modularLanguages.deltas.bytecode.coreInstructions._
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.integers.integerCompare.IfIntegerCompareGreaterOrEqualDelta
import miksilo.modularLanguages.deltas.bytecode.coreInstructions.integers.{IncrementIntegerDelta, LoadIntegerDelta, SmallIntegerConstantDelta, StoreIntegerDelta}
import miksilo.modularLanguages.deltas.bytecode.simpleBytecode.{LabelDelta, LabelledLocations}
import miksilo.modularLanguages.deltas.bytecode.types.IntTypeDelta
import miksilo.modularLanguages.deltas.javac.classes.ConstantPool
import miksilo.modularLanguages.util.TestLanguageBuilder
import miksilo.modularLanguagesutil.LanguageTest
import org.scalatest.funsuite.AnyFunSuite

class TestByteCodeGoTo extends AnyFunSuite {

  def testMain(instructions: Seq[Node]): Node = {
    val method = ByteCodeMethodInfo.methodInfo(0, 0, Seq(CodeAttributeDelta.codeAttribute(0, 0, 0, instructions, Seq(), Seq())))
    ByteCodeSkeleton.neww(2, 3, new ConstantPool(), Seq(method))
  }

  test("compareCompiledVersusNativeCode") {
    val labelledWhile = getLabelledJumpWhile
    val language = TestLanguageBuilder.build(Seq(LabelledLocations) ++ ByteCodeLanguage.byteCodeDeltas)
    val compiledWhile = language.compileAst(labelledWhile).program.asInstanceOf[PathRoot].current
    val expectedCode = getExpectedJumpWhile
    LanguageTest.testInstructionEquivalence(expectedCode, compiledWhile)
  }

  def getExpectedJumpWhile: Node = {
    val instructions = Seq(
      SmallIntegerConstantDelta.integerConstant(0),
      StoreIntegerDelta.integerStore(0),
      LoadIntegerDelta.load(0),
      SmallIntegerConstantDelta.integerConstant(3),
      IfIntegerCompareGreaterOrEqualDelta.ifIntegerCompareGreater(9),
      IncrementIntegerDelta.integerIncrement(0, 1),
      GotoDelta.goTo(-8))

    val stackMapTable = StackMapTableAttributeDelta.stackMapTable(1, Seq(StackMapTableAttributeDelta.appendFrame(2, Seq(IntTypeDelta.intType)),
      StackMapTableAttributeDelta.sameFrame(10)))
    val method = ByteCodeMethodInfo.methodInfo(0, 0, Seq(CodeAttributeDelta.codeAttribute(0, 0, 0, instructions, Seq(), Seq(stackMapTable))))
    ByteCodeSkeleton.neww(2, 3, new ConstantPool(Seq(StackMapTableAttributeDelta.entry)), Seq(method))
  }

  def getLabelledJumpWhile: Node = {
    val instructions = Seq(
      SmallIntegerConstantDelta.integerConstant(0),
      StoreIntegerDelta.integerStore(0),
      LabelDelta.label("start", new Node(StackMapTableAttributeDelta.AppendFrame,
        StackMapTableAttributeDelta.AppendFrameTypes -> Seq(IntTypeDelta.intType))),
      LoadIntegerDelta.load(0),
      SmallIntegerConstantDelta.integerConstant(3),
      LabelledLocations.ifIntegerCompareGreaterEquals("end"),
      IncrementIntegerDelta.integerIncrement(0, 1),
      LabelledLocations.goTo("start"),
      LabelDelta.label("end", new Node(StackMapTableAttributeDelta.SameFrameKey))
    )

    val method = ByteCodeMethodInfo.methodInfo(0, 0, Seq(CodeAttributeDelta.codeAttribute(0, 0, 0, instructions, Seq(), Seq())))
    ByteCodeSkeleton.neww(2, 3, new ConstantPool(), Seq(method))
  }
}
