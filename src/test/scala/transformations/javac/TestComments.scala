package transformations.javac

import application.compilerBuilder.PresetsPanel
import core.bigrammar.TestGrammarUtils
import core.particles.CompilerFromParticles
import org.junit.{Assert, Test}
import util.TestUtils

import scala.reflect.io.{File, Path}

class TestComments extends TestUtils(new CompilerFromParticles(Seq(JavaStyleCommentsC) ++ JavaCompiler.javaCompilerTransformations)) {

  val testGrammar = TestGrammarUtils(this.compiler.particles)

  test("BasicClass") {
    val input = "/* jooo */"
    TestGrammarUtils.parseAndPrintSame(input, None, JavaStyleCommentsC.getCommentGrammar)
  }

  test("comparePrintResultWithoutComment") {
    val testFile: File = getJavaTestFile("Whilee")
    val input = testFile.slurp()
    val result = testGrammar.parseAndPrint(input, None, TestGrammarUtils.getGrammarUsingTransformer())
    assertResult(input)( result)
  }

  test("comparePrintResult") {
    val testFile: File = getJavaTestFile("WhileeWithComment.java")
    val input = testFile.slurp()
    val result = testGrammar.parseAndPrint(input, None, testGrammar.getGrammarUsingTransformer())
    assertResult(input)( result)
  }

  test("FullPipeline") {
    val inputDirectory = Path("")
    val output: String = compileAndRun("WhileeWithComment.java", inputDirectory)
    assertResult("3")( output)
  }

  test("javaToSimplified") {
    val initialCompiler = new CompilerFromParticles(PresetsPanel.getJavaCompilerParticles)
    val utils = new TestUtils(new CompilerFromParticles(Seq(JavaStyleCommentsC) ++ initialCompiler.spliceBeforeTransformations(JavaCompiler.byteCodeTransformations, Seq(JavaStyleCommentsC))))
    val result = utils.compileAndPrettyPrint(utils.getJavaTestFile("FibonacciWithComments.java"))
    val expectedResult = utils.getTestFile("FibonacciWithCommentsBytecode.txt").slurp()
    assertResult(expectedResult)(result)
  }
}
