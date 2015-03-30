package transformations.bytecode.readJar

import java.io.BufferedInputStream

import application.compilerCockpit.PrettyPrint
import core.particles.CompilerFromParticles
import org.junit.{Assert, Test}
import transformations.javac.JavaCompiler
import util.TestUtils

class TestClassFileParser {

  @Test
  def testObjectClassUnparsedAttributes() = {

    val file = TestUtils.getTestFile("Object.class")
    val bis = new BufferedInputStream(file.inputStream())
    val inputBytes = Stream.continually(bis.read).takeWhile(-1 !=).map(_.toByte)
    val parseResult = ClassFileParser.classFileParser(new ArrayReader(0, inputBytes))
    val clazz = parseResult.get
    val state = new CompilerFromParticles(Seq(new PrettyPrint()) ++ ClassFileSignatureDecompiler.byteCodeParticles).transformReturnState(clazz)

    val expected = TestUtils.getTestFile("DecodedObjectClassPrettyPrint.txt").slurp()
    Assert.assertEquals(expected, state.output)
  }

  @Test
  def testObjectClassParsedAttributes() = {

    val file = TestUtils.getTestFile("Object.class")
    val bis = new BufferedInputStream(file.inputStream())
    val inputBytes = Stream.continually(bis.read).takeWhile(-1 !=).map(_.toByte)
    val parseResult = ClassFileParser.classFileParser(new ArrayReader(0, inputBytes))
    val clazz = parseResult.get
    val state = new CompilerFromParticles(Seq(ParseAttributes) ++ Seq(new PrettyPrint()) ++ ClassFileSignatureDecompiler.byteCodeParticles).transformReturnState(clazz)

    val expected = TestUtils.getTestFile("DecodedWithAttributesObjectClassPrettyPrint.txt").slurp()
    Assert.assertEquals(expected, state.output)
  }

  @Test
  def testObjectClassSignatureDeCompilation() = {

    val file = TestUtils.getTestFile("Object.class")
    val bis = new BufferedInputStream(file.inputStream())
    val inputBytes = Stream.continually(bis.read).takeWhile(-1 !=).map(_.toByte)
    val parseResult = ClassFileParser.classFileParser(new ArrayReader(0, inputBytes))
    val clazz = parseResult.get
    val state = new CompilerFromParticles(Seq(ParseAttributes, DecompileByteCodeSignature) ++ ClassFileSignatureDecompiler.byteCodeParticles).transformReturnState(clazz)
    val outputState = new CompilerFromParticles(Seq(new PrettyPrint()) ++ JavaCompiler.javaCompilerTransformations).transformReturnState(state.program)

    //val expected = TestUtils.getTestFile("DecodedWithAttributesObjectClassPrettyPrint.txt").slurp()
    Assert.assertEquals("", outputState.output)
  }
}
