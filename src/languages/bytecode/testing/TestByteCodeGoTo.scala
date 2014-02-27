package languages.bytecode.testing

import org.junit.{Assert, Test}
import languages.bytecode.{PrintByteCode, ByteCode, ByteCodeGoTo}
import transformation.{TransformationManager, MetaObject}
import akka.util.Convert

class TestByteCodeGoTo {

  def testMain(instructions: Seq[MetaObject]) : MetaObject = {
    val method = ByteCode.methodInfo(0, 0, Seq(ByteCode.codeAttribute(0,0,0,0,instructions,Seq(),Seq())))
    ByteCode.clazz(2, 3, Seq(), Seq(method))
  }

  @Test
  def compareCompiledVersusNativeCode() {
    val labelledWhile = getLabelledJumpWhile
    val compiledWhile = TransformationManager.buildCompiler(Seq(ByteCodeGoTo)).compile(labelledWhile)
    val nativeCode= getIndexedJumpWhile
    Assert.assertTrue(MetaObject.deepEquality(compiledWhile,nativeCode))
  }

  def getIndexedJumpWhile: MetaObject = testMain(Seq(
    ByteCode.integerConstant(0),
    ByteCode.addressStore(0),
    ByteCode.addressLoad(0),
    ByteCode.integerConstant(3),
    ByteCode.ifIntegerCompareGreater(13),
    ByteCode.integerIncrement(0, 1),
    ByteCode.goTo(2)))

  def getLabelledJumpWhile: MetaObject = testMain(Seq(
    ByteCode.integerConstant(0),
    ByteCode.addressStore(0),
    ByteCodeGoTo.label("start"),
    ByteCode.addressLoad(0),
    ByteCode.integerConstant(3),
    ByteCodeGoTo.ifIntegerCompareGreater("end"),
    ByteCode.integerIncrement(0, 1),
    ByteCodeGoTo.goTo("start"),
    ByteCodeGoTo.label("end")))
}
