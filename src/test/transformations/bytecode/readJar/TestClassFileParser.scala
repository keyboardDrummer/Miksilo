package transformations.bytecode.readJar

import java.io.BufferedInputStream

import application.compilerCockpit.PrettyPrint
import core.particles.CompilerFromParticles
import core.particles.node.Node
import org.junit.{Assert, Test}
import transformations.javac.JavaCompiler
import util.TestUtils

import scala.reflect.io.Path

class TestClassFileParser {

  @Test
  def testObjectClassUnparsedAttributes() = {
    val clazz: Node = decodeFile("Object.class")
    val state = new CompilerFromParticles(Seq(new PrettyPrint()) ++ ClassFileSignatureDecompiler.byteCodeParticles).transformReturnState(clazz)

    val expected = TestUtils.getTestFile("DecodedObjectClassPrettyPrint.txt").slurp()
    Assert.assertEquals(expected, state.output)
  }

  @Test
  def testObjectClassParsedAttributes() = {
    val clazz: Node = decodeFile("Object.class")
    val state = new CompilerFromParticles(Seq(ParseKnownAttributes) ++ Seq(new PrettyPrint()) ++
      ClassFileSignatureDecompiler.decompilerByteCodeParticles).transformReturnState(clazz)

    val expected = TestUtils.getTestFile("DecodedWithAttributesObjectClassPrettyPrint.txt").slurp()
    Assert.assertEquals(expected, state.output)
  }

  @Test
  def testObjectClassSignatureDeCompilation() = {
    val clazz: Node = decodeFile("Object.class")
    val state = new CompilerFromParticles(ClassFileSignatureDecompiler.getDecompiler).transformReturnState(clazz)
    val outputState = new CompilerFromParticles(Seq(new PrettyPrint()) ++ JavaCompiler.javaCompilerTransformations).transformReturnState(state.program)

    val expected = TestUtils.getTestFile("DecompiledClassFileSignature.txt").slurp()
    Assert.assertEquals(expected, outputState.output)
  }

  @Test
  def testSystemClassSignatureDeCompilation() = {
    val clazz: Node = decodeFile("System.class")
    val state = new CompilerFromParticles(ClassFileSignatureDecompiler.getDecompiler).transformReturnState(clazz)
    val outputState = new CompilerFromParticles(Seq(new PrettyPrint()) ++ JavaCompiler.javaCompilerTransformations).transformReturnState(state.program)

    val expected = ""//TestUtils.getTestFile("DecompiledClassFileSignature.txt").slurp()
    Assert.assertEquals(expected, outputState.output)
  }

  @Test
  def testPrintStreamClassSignatureDeCompilation() = {
    val clazz: Node = decodeFile("PrintStream.class")
    val state = new CompilerFromParticles(ClassFileSignatureDecompiler.getDecompiler).transformReturnState(clazz)
    val outputState = new CompilerFromParticles(Seq(new PrettyPrint()) ++ JavaCompiler.javaCompilerTransformations).transformReturnState(state.program)

    val expected = ""//TestUtils.getTestFile("DecompiledClassFileSignature.txt").slurp()
    Assert.assertEquals(expected, outputState.output)
  }

  def decodeFile(fileName: Path): Node = {
    val file = TestUtils.getTestFile(fileName)
    val bis = new BufferedInputStream(file.inputStream())
    val inputBytes = Stream.continually(bis.read).takeWhile(-1 !=).map(_.toByte)
    val parseResult = ClassFileParser.classFileParser(new ArrayReader(0, inputBytes))
    val clazz = parseResult.get
    clazz
  }
}
