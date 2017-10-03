package transformations.bytecode

import core.particles.node.Node
import org.scalatest.FunSuite
import transformations.bytecode.additions.LabelledLocations
import transformations.bytecode.attributes.{CodeAttribute, StackMapTableAttribute}
import transformations.bytecode.coreInstructions._
import transformations.bytecode.coreInstructions.integers.integerCompare.IfIntegerCompareGreaterOrEqualDelta
import transformations.bytecode.coreInstructions.integers.{IncrementIntegerDelta, LoadIntegerDelta, SmallIntegerConstantDelta, StoreIntegerDelta}
import transformations.bytecode.types.IntTypeC
import transformations.javac.JavaCompiler
import transformations.javac.classes.ConstantPool
import util.CompilerBuilder
import util.TestUtils

class TestByteCodeGoTo extends FunSuite {

  def testMain(instructions: Seq[Node]): Node = {
    val method = ByteCodeMethodInfo.methodInfo(0, 0, Seq(CodeAttribute.codeAttribute(0, 0, 0, instructions, Seq(), Seq())))
    ByteCodeSkeleton.clazz(2, 3, new ConstantPool(), Seq(method))
  }

  test("compareCompiledVersusNativeCode") {
    val labelledWhile = getLabelledJumpWhile
    val compiledWhile = CompilerBuilder.build(Seq(LabelledLocations) ++ JavaCompiler.byteCodeTransformations).transform(labelledWhile).program
    val expectedCode = getExpectedJumpWhile
    TestUtils.testInstructionEquivalence(compiledWhile, expectedCode)
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

    val stackMapTable = StackMapTableAttribute.stackMapTable(1, Seq(StackMapTableAttribute.appendFrame(2, Seq(IntTypeC.intType)),
      StackMapTableAttribute.sameFrame(10)))
    val method = ByteCodeMethodInfo.methodInfo(0, 0, Seq(CodeAttribute.codeAttribute(0, 0, 0, instructions, Seq(), Seq(stackMapTable))))
    ByteCodeSkeleton.clazz(2, 3, new ConstantPool(Seq(StackMapTableAttribute.entry)), Seq(method))
  }

  def getLabelledJumpWhile: Node = {
    val instructions = Seq(
      SmallIntegerConstantDelta.integerConstant(0),
      StoreIntegerDelta.integerStore(0),
      LabelledLocations.label("start", new Node(StackMapTableAttribute.AppendFrame,
        StackMapTableAttribute.AppendFrameTypes -> Seq(IntTypeC.intType))),
      LoadIntegerDelta.load(0),
      SmallIntegerConstantDelta.integerConstant(3),
      LabelledLocations.ifIntegerCompareGreaterEquals("end"),
      IncrementIntegerDelta.integerIncrement(0, 1),
      LabelledLocations.goTo("start"),
      LabelledLocations.label("end", new Node(StackMapTableAttribute.SameFrameKey))
    )

    val method = ByteCodeMethodInfo.methodInfo(0, 0, Seq(CodeAttribute.codeAttribute(0, 0, 0, instructions, Seq(), Seq())))
    ByteCodeSkeleton.clazz(2, 3, new ConstantPool(), Seq(method))
  }
}
