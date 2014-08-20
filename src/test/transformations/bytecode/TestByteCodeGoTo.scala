package transformations.bytecode

import core.transformation.{MetaObject, Transformer}
import org.junit.Test
import transformations.bytecode.coreInstructions._
import transformations.bytecode.coreInstructions.integers.integerCompare.IfIntegerCompareGreaterOrEqualC
import transformations.bytecode.coreInstructions.integers.{IncrementIntegerC, IntegerConstantC, StoreIntegerC, LoadIntegerC}
import transformations.javac.JavaCompiler
import transformations.types.IntTypeC
import util.TestUtils

import scala.collection.mutable

class TestByteCodeGoTo {

  def testMain(instructions: Seq[MetaObject]): MetaObject = {
    val method = ByteCodeSkeleton.methodInfo(0, 0, Seq(ByteCodeSkeleton.codeAttribute(0, 0, 0, instructions, Seq(), Seq())))
    ByteCodeSkeleton.clazz(2, 3, mutable.Buffer[Any](), Seq(method))
  }

  @Test
  def compareCompiledVersusNativeCode() {
    val labelledWhile = getLabelledJumpWhile
    val compiledWhile = new Transformer(Seq(LabelledTargets) ++ JavaCompiler.byteCodeTransformations).transform(labelledWhile)
    val expectedCode = getExpectedJumpWhile
    TestUtils.testInstructionEquivalence(compiledWhile, expectedCode)
  }

  def getExpectedJumpWhile: MetaObject = {
    val instructions = Seq(
      IntegerConstantC.integerConstant(0),
      StoreIntegerC.integerStore(0),
      LoadIntegerC.load(0),
      IntegerConstantC.integerConstant(3),
      IfIntegerCompareGreaterOrEqualC.ifIntegerCompareGreater(9),
      IncrementIntegerC.integerIncrement(0, 1),
      GotoC.goTo(-8))

    val stackMapTable = ByteCodeSkeleton.stackMapTable(1, Seq(ByteCodeSkeleton.appendFrame(2, Seq(IntTypeC.intType)),
      ByteCodeSkeleton.sameFrame(10)))
    val method = ByteCodeSkeleton.methodInfo(0, 0, Seq(ByteCodeSkeleton.codeAttribute(0, 0, 0, instructions, Seq(), Seq(stackMapTable))))
    ByteCodeSkeleton.clazz(2, 3, mutable.Buffer[Any](ByteCodeSkeleton.StackMapTableId), Seq(method))
  }

  def getLabelledJumpWhile: MetaObject = {
    val instructions = Seq(
      IntegerConstantC.integerConstant(0),
      StoreIntegerC.integerStore(0),
      LabelledTargets.label("start", new MetaObject(ByteCodeSkeleton.AppendFrame) {
        data.put(ByteCodeSkeleton.AppendFrameTypes, Seq(IntTypeC.intType))
      }),
      LoadIntegerC.load(0),
      IntegerConstantC.integerConstant(3),
      LabelledTargets.ifIntegerCompareGreaterEquals("end"),
      IncrementIntegerC.integerIncrement(0, 1),
      LabelledTargets.goTo("start"),
      LabelledTargets.label("end", new MetaObject(ByteCodeSkeleton.SameFrameKey))
    )

    val method = ByteCodeSkeleton.methodInfo(0, 0, Seq(ByteCodeSkeleton.codeAttribute(0, 0, 0, instructions, Seq(), Seq())))
    ByteCodeSkeleton.clazz(2, 3, mutable.Buffer[Any](), Seq(method))
  }
}
