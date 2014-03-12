package languages.bytecode.testing

import org.junit.{Assert, Test}
import languages.bytecode.{PrintByteCode, ByteCode, ByteCodeGoTo}
import transformation.{TransformationManager, MetaObject}
import akka.util.Convert
import scala.collection.mutable
import languages.javac.base.JavaTypes
import languages.javac.testing.TestUtils

class TestByteCodeGoTo {

  def testMain(instructions: Seq[MetaObject]) : MetaObject = {
    val method = ByteCode.methodInfo(0, 0, Seq(ByteCode.codeAttribute(0,0,0,instructions,Seq(),Seq())))
    ByteCode.clazz(2, 3, mutable.Buffer[Any](), Seq(method))
  }

  @Test
  def compareCompiledVersusNativeCode() {
    val labelledWhile = getLabelledJumpWhile
    val compiledWhile = TransformationManager.buildCompiler(Seq(ByteCodeGoTo)).compile(labelledWhile)
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

    val stackMapTable = ByteCode.stackMapTable(1, Seq(ByteCode.appendFrame(2,Seq(JavaTypes.IntegerType)),
      ByteCode.sameFrame(10)))
    val method = ByteCode.methodInfo(0, 0, Seq(ByteCode.codeAttribute(0,0,0,instructions,Seq(),Seq(stackMapTable))))
    ByteCode.clazz(2, 3, mutable.Buffer[Any](ByteCode.StackMapTableId), Seq(method))
  }

  def getLabelledJumpWhile: MetaObject = {
    val instructions = Seq(
      ByteCode.integerConstant(0),
    ByteCode.integerStore(0),
    ByteCodeGoTo.label("start", new MetaObject(ByteCode.AppendFrame)
    {
      data.put(ByteCode.AppendFrameTypes, Seq(JavaTypes.IntegerType))
    }),
    ByteCode.integerLoad(0),
    ByteCode.integerConstant(3),
    ByteCodeGoTo.ifIntegerCompareGreater("end"),
    ByteCode.integerIncrement(0, 1),
    ByteCodeGoTo.goTo("start"),
    ByteCodeGoTo.label("end", new MetaObject(ByteCode.SameFrameKey))
    )

    val method = ByteCode.methodInfo(0, 0, Seq(ByteCode.codeAttribute(0,0,0,instructions,Seq(),Seq())))
    ByteCode.clazz(2, 3, mutable.Buffer[Any](), Seq(method))
  }
}
