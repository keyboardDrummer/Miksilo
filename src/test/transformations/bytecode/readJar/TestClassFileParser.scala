package transformations.bytecode.readJar

import java.io.BufferedInputStream

import application.compilerCockpit.PrettyPrint
import core.particles.CompilerFromParticles
import org.junit.{Assert, Test}
import util.TestUtils

import scala.util.parsing.input.{Position, Reader}

class TestClassFileParser {

  @Test
  def testObjectClass() = {

    val file = TestUtils.getTestFile("Object.class")
    val bis = new BufferedInputStream(file.inputStream())
    val inputBytes = Stream.continually(bis.read).takeWhile(-1 !=).map(_.toByte).toArray
    val parseResult = ClassFileParser.classFileParser(new ArrayReader(0, inputBytes))
    val clazz = parseResult.get
    val state = new CompilerFromParticles(Seq(new PrettyPrint()) ++ ClassFileSignatureDecompiler.getDecompiler).transformReturnState(clazz)

    val expected = TestUtils.getTestFile("DecodedObjectClassPrettyPrint.txt").slurp()
    Assert.assertEquals(expected, state.output)
  }

  class ArrayReader(offset: Int, bytes: Array[Byte]) extends Reader[Byte] {
    override def first: Byte = bytes(offset)

    override def atEnd: Boolean = offset >= bytes.length

    override def pos: Position = new Position {
      override def line: Int = 0

      override def column: Int = offset

      override protected def lineContents: String = "some byte"
    }

    override def rest: Reader[Byte] = new ArrayReader(offset+1, bytes)
  }
}
