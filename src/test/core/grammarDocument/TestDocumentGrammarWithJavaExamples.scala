package core.grammarDocument

import application.compilerCockpit.{OutputOption, ParseFromFunction, PrettyPrint}
import core.modularProgram.PieceCombiner
import core.transformation.TransformationState
import core.transformation.sillyCodePieces.Injector
import org.junit.{Assert, Test}
import transformations.javac.constructor.{DefaultConstructorC, ImplicitSuperConstructorCall}
import transformations.javac.expressions.TernaryC
import transformations.javac.methods.{ImplicitReturnAtEndOfMethod, MethodC}
import transformations.javac.statements.BlockC
import transformations.javac.{ImplicitJavaLangImport, ImplicitObjectSuperClass, ImplicitThisInPrivateCalls, JavaCompiler}
import util.TestUtils

import scala.reflect.io.Path

class TestDocumentGrammarWithJavaExamples {
  val lineSeparator = System.lineSeparator()

  @Test
  def testSimpleForLoop() {
    val testFileContent = TestUtils.getTestFile("SimpleForLoop", Path("")).slurp()
    TestGrammarUtils.compareInputWithPrint(testFileContent, None)
  }

  @Test
  def testWhile() {
    val testFileContent = TestUtils.getTestFile("whilee", Path("")).slurp()
    TestGrammarUtils.compareInputWithPrint(testFileContent, None)
  }

  @Test
  def testFibonacci() {
    val testFileContent = TestUtils.getTestFile("fibonacci", Path("")).slurp()
    TestGrammarUtils.compareInputWithPrint(testFileContent, None)
  }

  @Test
  def testTernary() {
    val input = "1 ? 2 : 3"
    TestGrammarUtils.compareInputWithPrint(input, None, TernaryC.TernaryExpressionGrammar)
  }

  @Test
  def testFibonacciMainMethod() {
    val input = s"public static void main(java.lang.String[] args)$lineSeparator{$lineSeparator    System.out.print(fibonacci(5));$lineSeparator}"
    TestGrammarUtils.compareInputWithPrint(input, None, MethodC.MethodGrammar)
  }

  @Test
  def testBlock() {
    val input = "{" + lineSeparator + "    System.out.print(fibonacci(5));" + lineSeparator + "}"
    TestGrammarUtils.compareInputWithPrint(input, None, BlockC.BlockGrammar)
  }

  @Test
  def testPrintAfterImplicitAddition() {
    val input = TestUtils.getTestFile("fibonacci", Path("")).slurp()
    val expectation = TestUtils.getTestFile("ExplicitFibonacci", Path("")).slurp()

    val implicits = Seq[Injector](ImplicitJavaLangImport, DefaultConstructorC, ImplicitSuperConstructorCall,
      ImplicitObjectSuperClass, ImplicitThisInPrivateCalls, ImplicitReturnAtEndOfMethod)
    val newTransformations = Seq(new ParseFromFunction(() => input)) ++ implicits ++ Seq(PrettyPrint) ++
      JavaCompiler.spliceAfterTransformations(implicits, Seq(PrettyPrint))

    val state = new TransformationState
    PieceCombiner.combineAndExecute(state, newTransformations.reverse)
    val output = OutputOption.getOutput(state).get

    Assert.assertEquals(expectation, output)
  }

  @Test
  def testPrettyPrintByteCode() {
    val input = TestUtils.getTestFile("fibonacci", Path("")).slurp()
    val expectation = TestUtils.getTestFile("ExplicitFibonacci", Path("")).slurp()

    val newTransformations = Seq(new ParseFromFunction(() => input)) ++
      JavaCompiler.spliceBeforeTransformations(JavaCompiler.byteCodeWithoutInstructions, Seq(PrettyPrint))

    val state = new TransformationState
    PieceCombiner.combineAndExecute(state, newTransformations.reverse)
    val output = OutputOption.getOutput(state).get

    Assert.assertEquals(expectation, output)
  }
}
