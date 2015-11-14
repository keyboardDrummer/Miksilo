package transformations.javac

import core.bigrammar.TestGrammarUtils
import org.junit.{Assert, Test}
import util.TestUtils

import scala.reflect.io.{File, Path}

class TestComments {

  @Test
  def testBasicClass() {
    val input = "/* jooo */"
    TestGrammarUtils.parseAndPrintSame(input, None, JavaCommentsC.getCommentGrammar)
  }

  @Test
  def comparePrintResultWithoutComment() {
    val testFile: File = TestUtils.getJavaTestFile("Whilee")
    val input = testFile.slurp()
    val result = TestGrammarUtils.parseAndPrint(input, None, TestGrammarUtils.getGrammarUsingTransformer())
    Assert.assertEquals(input, result)
  }

  @Test
  def comparePrintResult() {
    val testFile: File = TestUtils.getJavaTestFile("WhileeWithComment.java")
    val input = testFile.slurp()
    val result = TestGrammarUtils.parseAndPrint(input, None, TestGrammarUtils.getGrammarUsingTransformer())
    Assert.assertEquals(input, result)
  }

  @Test
  def testFullPipeline() {
    val inputDirectory = Path("")
    val output: String = TestUtils.compileAndRun("WhileeWithComment.java", inputDirectory)
    Assert.assertEquals("3", output)
  }
}
