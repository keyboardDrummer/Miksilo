package transformations.bytecode

import core.transformation.{MetaObject, Transformer}
import org.junit.Test
import transformations.bytecode.instructions._
import transformations.javac.base.model.JavaTypes
import transformations.javac.{JavaCompiler, TestUtils}

import scala.collection.mutable

class TestByteCodeGoTo {

  def testMain(instructions: Seq[MetaObject]): MetaObject = {
    val method = ByteCodeSkeleton.methodInfo(0, 0, Seq(ByteCodeSkeleton.codeAttribute(0, 0, 0, instructions, Seq(), Seq())))
    ByteCodeSkeleton.clazz(2, 3, mutable.Buffer[Any](), Seq(method))
  }

  @Test
  def compareCompiledVersusNativeCode() {
    val labelledWhile = getLabelledJumpWhile
    val compiledWhile = new Transformer(Seq(LabelledJumps) ++ JavaCompiler.byteCodeTransformations).transform(labelledWhile)
    val expectedCode = getExpectedJumpWhile
    TestUtils.testInstructionEquivalence(compiledWhile, expectedCode)
  }

  def getExpectedJumpWhile: MetaObject = {
    val instructions = Seq(
      IntegerConstantC.integerConstant(0),
      StoreIntegerC.integerStore(0),
      LoadIntegerC.integerLoad(0),
      IntegerConstantC.integerConstant(3),
      IfIntegerCompareGreaterC.ifIntegerCompareGreater(9),
      IncrementIntegerC.integerIncrement(0, 1),
      GotoC.goTo(-8))

    val stackMapTable = ByteCodeSkeleton.stackMapTable(1, Seq(ByteCodeSkeleton.appendFrame(2, Seq(JavaTypes.intType)),
      ByteCodeSkeleton.sameFrame(10)))
    val method = ByteCodeSkeleton.methodInfo(0, 0, Seq(ByteCodeSkeleton.codeAttribute(0, 0, 0, instructions, Seq(), Seq(stackMapTable))))
    ByteCodeSkeleton.clazz(2, 3, mutable.Buffer[Any](ByteCodeSkeleton.StackMapTableId), Seq(method))
  }

  def getLabelledJumpWhile: MetaObject = {
    val instructions = Seq(
      IntegerConstantC.integerConstant(0),
      StoreIntegerC.integerStore(0),
      LabelledJumps.label("start", new MetaObject(ByteCodeSkeleton.AppendFrame) {
        data.put(ByteCodeSkeleton.AppendFrameTypes, Seq(JavaTypes.intType))
      }),
      LoadIntegerC.integerLoad(0),
      IntegerConstantC.integerConstant(3),
      LabelledJumps.ifIntegerCompareGreater("end"),
      IncrementIntegerC.integerIncrement(0, 1),
      LabelledJumps.goTo("start"),
      LabelledJumps.label("end", new MetaObject(ByteCodeSkeleton.SameFrameKey))
    )

    val method = ByteCodeSkeleton.methodInfo(0, 0, Seq(ByteCodeSkeleton.codeAttribute(0, 0, 0, instructions, Seq(), Seq())))
    ByteCodeSkeleton.clazz(2, 3, mutable.Buffer[Any](), Seq(method))
  }
}
