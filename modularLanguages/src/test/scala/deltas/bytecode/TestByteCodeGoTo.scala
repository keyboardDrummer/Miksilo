package deltas.bytecode

import core.deltas.path.PathRoot
import core.language.node.Node
import deltas.bytecode.attributes.{CodeAttributeDelta, StackMapTableAttributeDelta}
import deltas.bytecode.coreInstructions._
import deltas.bytecode.coreInstructions.integers.integerCompare.IfIntegerCompareGreaterOrEqualDelta
import deltas.bytecode.coreInstructions.integers.{IncrementIntegerDelta, LoadIntegerDelta, SmallIntegerConstantDelta, StoreIntegerDelta}
import deltas.bytecode.simpleBytecode.{LabelDelta, LabelledLocations}
import deltas.bytecode.types.IntTypeDelta
import deltas.javac.ByteCodeLanguage
import deltas.javac.classes.ConstantPool
import org.scalatest.FunSuite
import util.{LanguageTest, TestLanguageBuilder}

class TestByteCodeGoTo extends FunSuite {

  def testMain(instructions: Seq[Node]): Node = {
    val method = ByteCodeMethodInfo.methodInfo(0, 0, Seq(CodeAttributeDelta.codeAttribute(0, 0, 0, instructions, Seq(), Seq())))
    ByteCodeSkeleton.neww(2, 3, new ConstantPool(), Seq(method))
  }

  test("compareCompiledVersusNativeCode") {
    val labelledWhile = getLabelledJumpWhile
    val language = TestLanguageBuilder.build(Seq(LabelledLocations) ++ ByteCodeLanguage.byteCodeDeltas)
    val compiledWhile = language.compileAst(labelledWhile).program.asInstanceOf[PathRoot].current
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
