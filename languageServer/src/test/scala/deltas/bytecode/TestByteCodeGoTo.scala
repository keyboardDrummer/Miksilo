package deltas.bytecode

import core.language.node.Node
import org.scalatest.FunSuite
import deltas.bytecode.attributes.{CodeAttributeDelta, StackMapTableAttribute}
import deltas.bytecode.coreInstructions._
import deltas.bytecode.coreInstructions.integers.integerCompare.IfIntegerCompareGreaterOrEqualDelta
import deltas.bytecode.coreInstructions.integers.{IncrementIntegerDelta, LoadIntegerDelta, SmallIntegerConstantDelta, StoreIntegerDelta}
import deltas.bytecode.simpleBytecode.{LabelDelta, LabelledLocations}
import deltas.bytecode.types.IntTypeDelta
import deltas.javac.JavaLanguage
import deltas.javac.classes.ConstantPool
import util.TestLanguageBuilder
import util.LanguageTest

class TestByteCodeGoTo extends FunSuite {

  def testMain(instructions: Seq[Node]): Node = {
    val method = ByteCodeMethodInfo.methodInfo(0, 0, Seq(CodeAttributeDelta.codeAttribute(0, 0, 0, instructions, Seq(), Seq())))
    ByteCodeSkeleton.neww(2, 3, new ConstantPool(), Seq(method))
  }

  test("compareCompiledVersusNativeCode") {
    val labelledWhile = getLabelledJumpWhile
    val compiledWhile = TestLanguageBuilder.build(Seq(LabelledLocations) ++ JavaLanguage.byteCodeDeltas).transform(labelledWhile).program
    val expectedCode = getExpectedJumpWhile
    LanguageTest.testInstructionEquivalence(compiledWhile, expectedCode)
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

    val stackMapTable = StackMapTableAttribute.stackMapTable(1, Seq(StackMapTableAttribute.appendFrame(2, Seq(IntTypeDelta.intType)),
      StackMapTableAttribute.sameFrame(10)))
    val method = ByteCodeMethodInfo.methodInfo(0, 0, Seq(CodeAttributeDelta.codeAttribute(0, 0, 0, instructions, Seq(), Seq(stackMapTable))))
    ByteCodeSkeleton.neww(2, 3, new ConstantPool(Seq(StackMapTableAttribute.entry)), Seq(method))
  }

  def getLabelledJumpWhile: Node = {
    val instructions = Seq(
      SmallIntegerConstantDelta.integerConstant(0),
      StoreIntegerDelta.integerStore(0),
      LabelDelta.label("start", new Node(StackMapTableAttribute.AppendFrame,
        StackMapTableAttribute.AppendFrameTypes -> Seq(IntTypeDelta.intType))),
      LoadIntegerDelta.load(0),
      SmallIntegerConstantDelta.integerConstant(3),
      LabelledLocations.ifIntegerCompareGreaterEquals("end"),
      IncrementIntegerDelta.integerIncrement(0, 1),
      LabelledLocations.goTo("start"),
      LabelDelta.label("end", new Node(StackMapTableAttribute.SameFrameKey))
    )

    val method = ByteCodeMethodInfo.methodInfo(0, 0, Seq(CodeAttributeDelta.codeAttribute(0, 0, 0, instructions, Seq(), Seq())))
    ByteCodeSkeleton.neww(2, 3, new ConstantPool(), Seq(method))
  }
}
