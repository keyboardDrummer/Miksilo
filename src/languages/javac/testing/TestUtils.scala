package languages.javac.testing

import transformation.{ComparisonOptions, MetaObject}
import languages.bytecode.{PrintByteCode, ByteCode}
import org.junit.Assert
import scala.reflect.io.{Path, File, Directory}
import scala.sys.process.{ProcessIO, Process}
import java.io._
import transformation.ComparisonOptions

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
    val tempDirectory = Directory.makeTemp()
    val file = File.apply(tempDirectory / Path(className).addExtension("class"))
    val writer = file.outputStream(false)
    writer.write(bytes)
    writer.close()

    val processBuilder = Process.apply(s"java ${className}", tempDirectory.jfile)
    var line: String = ""
    val output = (writeOutput: InputStream) => line += new BufferedReader(new InputStreamReader(writeOutput)).readLine()
    val processIO = new ProcessIO((writeInput: OutputStream) => {}, output, output)
    val exitValue = processBuilder.run(processIO).exitValue()
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
