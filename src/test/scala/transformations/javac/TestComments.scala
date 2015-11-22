package transformations.javac

import core.bigrammar.TestGrammarUtils
import core.particles.CompilerFromParticles
import org.junit.{Assert, Test}
import util.TestUtils

import scala.reflect.io.{File, Path}

class TestComments extends TestUtils(new CompilerFromParticles(Seq(JavaCommentsC) ++ JavaCompiler.javaCompilerTransformations)) {

  val testGrammar = TestGrammarUtils(this.compiler.particles)
  @Test
  def testBasicClass() {
    val input = "/* jooo */"
    TestGrammarUtils.parseAndPrintSame(input, None, JavaCommentsC.getCommentGrammar)
  }

  @Test
  def comparePrintResultWithoutComment() {
    val testFile: File = getJavaTestFile("Whilee")
    val input = testFile.slurp()
    val result = testGrammar.parseAndPrint(input, None, TestGrammarUtils.getGrammarUsingTransformer())
    Assert.assertEquals(input, result)
  }

  @Test
  def comparePrintResult() {
    val testFile: File = getJavaTestFile("WhileeWithComment.java")
    val input = testFile.slurp()
    val result = testGrammar.parseAndPrint(input, None, testGrammar.getGrammarUsingTransformer())
    Assert.assertEquals(input, result)
  }

  @Test
  def testFullPipeline() {
    val inputDirectory = Path("")
    val output: String = compileAndRun("WhileeWithComment.java", inputDirectory)
    Assert.assertEquals("3", output)
  }
}
