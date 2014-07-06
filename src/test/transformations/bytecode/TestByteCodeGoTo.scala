package transformations.bytecode.testing

import core.transformation.{MetaObject, TransformationManager}
import org.junit.Test
import transformations.bytecode.{ByteCode, LabelledJumps}
import transformations.javac.TestUtils
import transformations.javac.base.model.JavaTypes

import scala.collection.mutable

class TestByteCodeGoTo {

  def testMain(instructions: Seq[MetaObject]) : MetaObject = {
    val method = ByteCode.methodInfo(0, 0, Seq(ByteCode.codeAttribute(0,0,0,instructions,Seq(),Seq())))
    ByteCode.clazz(2, 3, mutable.Buffer[Any](), Seq(method))
  }

  @Test
  def compareCompiledVersusNativeCode() {
    val labelledWhile = getLabelledJumpWhile
    val compiledWhile = TransformationManager.buildCompiler(Seq(LabelledJumps)).compile(labelledWhile)
    val expectedCode = getExpectedJumpWhile
    TestUtils.testInstructionEquivalence(compiledWhile,expectedCode)
  }

  def getExpectedJumpWhile: MetaObject = {
    val instructions = Seq(
    ByteCode.integerConstant(0),
    ByteCode.integerStore(0),
    ByteCode.integerLoad(0),
    ByteCode.integerConstant(3),
    ByteCode.ifIntegerCompareGreater(9),
    ByteCode.integerIncrement(0, 1),
    ByteCode.goTo(-8))

    val stackMapTable = ByteCode.stackMapTable(1, Seq(ByteCode.appendFrame(2,Seq(JavaTypes.IntType)),
      ByteCode.sameFrame(10)))
    val method = ByteCode.methodInfo(0, 0, Seq(ByteCode.codeAttribute(0,0,0,instructions,Seq(),Seq(stackMapTable))))
    ByteCode.clazz(2, 3, mutable.Buffer[Any](ByteCode.StackMapTableId), Seq(method))
  }

  def getLabelledJumpWhile: MetaObject = {
    val instructions = Seq(
      ByteCode.integerConstant(0),
    ByteCode.integerStore(0),
    LabelledJumps.label("start", new MetaObject(ByteCode.AppendFrame)
    {
      data.put(ByteCode.AppendFrameTypes, Seq(JavaTypes.IntType))
    }),
    ByteCode.integerLoad(0),
    ByteCode.integerConstant(3),
    LabelledJumps.ifIntegerCompareGreater("end"),
    ByteCode.integerIncrement(0, 1),
    LabelledJumps.goTo("start"),
    LabelledJumps.label("end", new MetaObject(ByteCode.SameFrameKey))
    )

    val method = ByteCode.methodInfo(0, 0, Seq(ByteCode.codeAttribute(0,0,0,instructions,Seq(),Seq())))
    ByteCode.clazz(2, 3, mutable.Buffer[Any](), Seq(method))
  }
}
