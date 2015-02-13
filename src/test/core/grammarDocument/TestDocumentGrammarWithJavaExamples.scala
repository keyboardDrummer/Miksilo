package core.grammarDocument

import application.compilerCockpit._
import core.transformation.CompilerFromParticles
import core.transformation.sillyCodePieces.{GrammarTransformation, Particle}
import org.junit.{Assert, Test}
import transformations.bytecode.coreInstructions.objects.LoadAddressC
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
    val testFileContent = TestUtils.getJavaTestFile("SimpleForLoop", Path("")).slurp()
    TestGrammarUtils.compareInputWithPrint(testFileContent, None)
  }

  @Test
  def testWhile() {
    val testFileContent = TestUtils.getJavaTestFile("whilee", Path("")).slurp()
    TestGrammarUtils.compareInputWithPrint(testFileContent, None)
  }

  @Test
  def testFibonacci() {
    val testFileContent = TestUtils.getJavaTestFile("fibonacci", Path("")).slurp()
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
    val input = TestUtils.getJavaTestFile("fibonacci", Path("")).slurp()
    val expectation = TestUtils.getJavaTestFile("ExplicitFibonacci", Path("")).slurp()

    val implicits = Seq[Particle](ImplicitJavaLangImport, DefaultConstructorC, ImplicitSuperConstructorCall,
      ImplicitObjectSuperClass, ImplicitThisInPrivateCalls, ImplicitReturnAtEndOfMethod)
    val newTransformations = JavaCompiler.spliceAfterTransformations(implicits, Seq(PrettyPrint))

    val state = new CompilerFromParticles(newTransformations).parseAndTransform(input)
    val output = state.output

    Assert.assertEquals(expectation, output)
  }

  @Test
  def testPrettyPrintByteCode() {
    val input = TestUtils.getJavaTestFile("fibonacci", Path("")).slurp()
    val expectation = TestUtils.getTestFile("FibonacciByteCodePrettyPrinted.txt").slurp()

    val newTransformations = JavaCompiler.spliceBeforeTransformations(JavaCompiler.byteCodeTransformations, Seq(PrettyPrint))

    val state = new CompilerFromParticles(newTransformations).parseAndTransform(input)
    Assert.assertEquals(expectation, state.output)
  }


  @Test
  def testPrettyPrintAndParseByteCode() {
    val input = TestUtils.getJavaTestFile("fibonacci", Path("")).slurp()

    val byteCodeTransformations: Seq[GrammarTransformation] = JavaCompiler.byteCodeTransformations
    val prettyPrintTransformations = JavaCompiler.spliceBeforeTransformations(byteCodeTransformations, Seq(PrettyPrint))

    val state = new CompilerFromParticles(prettyPrintTransformations).parseAndTransform(input)
    val byteCode = state.output

    val parseTransformations = byteCodeTransformations ++ Seq(RunWithJVM)
    val output = new CompilerFromParticles(parseTransformations).parseAndTransform(byteCode).output
    Assert.assertEquals("8", output)
  }

  @Test
  def prettyPrintByteCode() {
    val input = TestUtils.getTestFile("FibonacciByteCodePrettyPrinted.txt").slurp()
    val parseTransformations = Seq(PrettyPrint) ++ JavaCompiler.byteCodeTransformations
    val output = new CompilerFromParticles(parseTransformations).parseAndTransform(input).output
    Assert.assertEquals(input, output)
  }

  @Test
  def parseByteCode() {
    val input = TestUtils.getTestFile("FibonacciByteCodePrettyPrinted.txt").slurp()
    val parseTransformations = JavaCompiler.byteCodeTransformations ++ Seq(RunWithJVM)
    val output = new CompilerFromParticles(parseTransformations).parseAndTransform(input).output
    Assert.assertEquals("8", output)
  }

  @Test
  def testPrettyAddressLoad(): Unit = {
    val grammar = LoadAddressC.getGrammarForThisInstruction
    val addressLoad = LoadAddressC.addressLoad(0)
    BiGrammarToDocument.toDocument(addressLoad, grammar)
  }
}
