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
    val input = getJavaTestFileContents("Whilee")
    val result = testGrammar.parseAndPrint(input, None, TestGrammarUtils.getGrammarUsingTransformer())
    assertResult(input)( result)
  }

  ignore("comparePrintResult") { //TODO fix
    val input = getJavaTestFileContents("WhileeWithComment.java")
    val result = testGrammar.parseAndPrint(input, None, testGrammar.getGrammarUsingTransformer())
    assertResult(input)( result)
  }

  ignore("FullPipeline") { //TODO fix
    val inputDirectory = Path("")
    val output: String = compileAndRun("WhileeWithComment.java", inputDirectory)
    assertResult("3")( output)
  }

  test("javaToSimplified") {
    val initialCompiler = new CompilerFromParticles(PresetsPanel.getJavaCompilerParticles)
    val utils = new TestUtils(new CompilerFromParticles(Seq(JavaStyleCommentsC) ++ initialCompiler.spliceBeforeTransformations(JavaCompiler.byteCodeTransformations, Seq(JavaStyleCommentsC))))
    val result = utils.compileAndPrettyPrint(utils.getJavaTestFileContents("FibonacciWithComments.java"))
    val expectedResult = utils.getTestFileContents("FibonacciWithCommentsByteCode.txt")
    assertResult(expectedResult)(result)
  }
}
