package transformations.javac

import core.transformation.{ComparisonOptions, MetaObject}
import transformations.bytecode.{PrintByteCode, ByteCode}
import org.junit.Assert
import scala.reflect.io.{Path, File}
import scala.sys.process.{ProcessLogger, Process}

object TestUtils {

  def getMethodInstructions(method: MetaObject) =
    ByteCode.getCodeInstructions(ByteCode.getMethodAttributes(method)(0))

  def testInstructionEquivalence(expectedByteCode: MetaObject, compiledCode: MetaObject) {
    for (methodPair <- ByteCode.getMethods(expectedByteCode).zip(ByteCode.getMethods(compiledCode))) {
      Assert.assertTrue(MetaObject.deepEquality(getMethodInstructions(methodPair._1),
        getMethodInstructions(methodPair._2),
        new ComparisonOptions(false, true, false)))
    }
  }

  def runByteCode(className: String, code: MetaObject, expectedResult: Int) {
    val line = runByteCode(className, code)
    Assert.assertEquals(expectedResult, Integer.parseInt(line))
  }


  def runByteCode(className: String, code: MetaObject) = {
    val bytes = PrintByteCode.getBytes(code).toArray
    val currentDir = new File(new java.io.File("."))
    val testDirectory = currentDir / Path("test")
    testDirectory.createDirectory()
    val byteCodeFile = File.apply(testDirectory / Path(className).addExtension("class"))
    val writer = byteCodeFile.outputStream(append = false)
    writer.write(bytes)
    writer.close()

    val processBuilder = Process.apply(s"java $className", testDirectory.jfile)
    var line: String = ""
    val logger = ProcessLogger(
      (o: String) => line += o,
      (e: String) => line += e)
    val exitValue = processBuilder ! logger
    Assert.assertEquals(0, exitValue)
    line
  }

  def compareConstantPools(expectedByteCode: MetaObject, compiledCode: MetaObject) {
    val expectedConstantPoolSet = ByteCode.getConstantPool(expectedByteCode)
    val compiledConstantPoolSet = ByteCode.getConstantPool(compiledCode)
    Assert.assertEquals(expectedConstantPoolSet.length, compiledConstantPoolSet.length)
    Assert.assertTrue(expectedConstantPoolSet.forall(expectedItem => {
      val hasEquivalent = compiledConstantPoolSet.exists(compiledItem => MetaObject.deepEquality(compiledItem, expectedItem,
        new ComparisonOptions(false, false, true)))
      hasEquivalent
    }))
  }
}
