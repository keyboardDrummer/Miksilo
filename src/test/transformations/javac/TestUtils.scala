package transformations.javac

import core.transformation._
import org.junit.Assert
import transformations.bytecode.{ByteCodeSkeleton, PrintByteCode}

import scala.reflect.io.{Directory, File, Path}
import scala.sys.process.{Process, ProcessLogger}

object TestUtils {

  def testInstructionEquivalence(expectedByteCode: MetaObject, compiledCode: MetaObject) {
    for (methodPair <- ByteCodeSkeleton.getMethods(expectedByteCode).zip(ByteCodeSkeleton.getMethods(compiledCode))) {
      Assert.assertTrue(MetaObject.deepEquality(getMethodInstructions(methodPair._1),
        getMethodInstructions(methodPair._2),
        new ComparisonOptions(false, true, false)))
    }
  }

  def getMethodInstructions(method: MetaObject) =
    ByteCodeSkeleton.getCodeInstructions(ByteCodeSkeleton.getMethodAttributes(method)(0))

  def printByteCode(byteCode: MetaObject): String = {
    PrintByteCode.printBytes(getBytes(byteCode))
  }

  def runByteCode(className: String, code: MetaObject, expectedResult: Int) {
    val line = runByteCode(className, code)
    Assert.assertEquals(expectedResult, Integer.parseInt(line))
  }

  def runByteCode(className: String, code: MetaObject) = {
    val bytes = getBytes(code).toArray
    val currentDir = new File(new java.io.File("."))
    val testDirectory = currentDir / Path("testOutput")
    testDirectory.createDirectory()
    val byteCodeFile = File.apply(testDirectory / Path(className).addExtension("class"))
    val writer = byteCodeFile.outputStream(append = false)
    writer.write(bytes)
    writer.close()

    runJavaClass(className, testDirectory)
  }

  def getBytes(byteCode: MetaObject): Seq[Byte] = {
    var output: Seq[Byte] = null
    val transformations: Seq[Injector] = JavaCompiler.byteCodeTransformations ++ Seq(new GetBytes(s => output = s))
    new Transformer(transformations).transform(byteCode)
    output
  }

  def runJavaClass(className: String, directory: Path): String = {
    val processBuilder = Process.apply(s"java $className", directory.jfile)
    var line: String = ""
    val logger = ProcessLogger(
      (o: String) => line += o,
      (e: String) => line += e)
    val exitValue = processBuilder ! logger
    Assert.assertEquals(line, 0, exitValue)
    line
  }

  def compileAndRun(className: String, inputDirectory: Path): String = {
    val relativeFilePath = inputDirectory / (className + ".java")
    val currentDir = new File(new java.io.File("."))
    val testOutput = Directory(currentDir / Path("testOutput"))
    val testResources = currentDir / Path("testResources")
    val input: File = File(testResources / relativeFilePath)
    JavaCompiler.getCompiler.compile(input, Directory(Path(testOutput.path) / inputDirectory))
    val qualifiedClassName: String = (inputDirectory / Path(className)).segments.reduce[String]((l, r) => l + "." + r)
    TestUtils.runJavaClass(qualifiedClassName, testOutput)
  }

  def compareConstantPools(expectedByteCode: MetaObject, compiledCode: MetaObject) {
    val expectedConstantPoolSet = ByteCodeSkeleton.getConstantPool(expectedByteCode)
    val compiledConstantPoolSet = ByteCodeSkeleton.getConstantPool(compiledCode)
    Assert.assertEquals(expectedConstantPoolSet.length, compiledConstantPoolSet.length)
    Assert.assertTrue(expectedConstantPoolSet.forall(expectedItem => {
      val hasEquivalent = compiledConstantPoolSet.exists(compiledItem => MetaObject.deepEquality(compiledItem, expectedItem,
        new ComparisonOptions(false, false, true)))
      hasEquivalent
    }))
  }

  class GetBytes(write: Seq[Byte] => Unit) extends ProgramTransformation {
    override def transform(program: MetaObject, state: TransformationState): Unit = {
      write(PrintByteCode.getBytes(program, state))
    }
  }

}
