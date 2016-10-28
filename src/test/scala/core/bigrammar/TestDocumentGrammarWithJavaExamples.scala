package core.bigrammar

import application.compilerCockpit._
import core.bigrammar.printer.BiGrammarToPrinter
import core.particles.grammars.GrammarCatalogue
import core.particles.{CompilerFromParticles, Particle}
import org.scalatest.{FunSpec, FunSuite}
import transformations.bytecode.coreInstructions.objects.LoadAddressC
import transformations.javac.constructor.{ConstructorC, DefaultConstructorC, ImplicitSuperConstructorCall}
import transformations.javac.expressions.TernaryC
import transformations.javac.methods.{ImplicitReturnAtEndOfMethod, MethodC}
import transformations.javac.statements.BlockC
import transformations.javac.{ImplicitJavaLangImport, ImplicitObjectSuperClass, ImplicitThisForPrivateMemberSelection, JavaCompiler}
import util.TestUtils

import scala.reflect.io.Path

class TestDocumentGrammarWithJavaExamples extends FunSuite {
  val lineSeparator = System.lineSeparator()


  def testSimpleForLoop() {
    val testFileContent = TestUtils.getJavaTestFile("SimpleForLoop", Path("")).slurp()
    TestGrammarUtils.compareInputWithPrint(testFileContent, None)
  }


  def testWhile() {
    val testFileContent = TestUtils.getJavaTestFile("whilee", Path("")).slurp()
    TestGrammarUtils.compareInputWithPrint(testFileContent, None)
  }

  def testFibonacci() {
    val testFileContent = TestUtils.getJavaTestFile("fibonacci", Path("")).slurp()
    TestGrammarUtils.compareInputWithPrint(testFileContent, None)
  }


  def testTernary() {
    val input = "1 ? 2 : 3"
    TestGrammarUtils.compareInputWithPrint(input, None, TernaryC.TernaryExpressionGrammar)
  }


  def testFibonacciMainMethod() {
    val input = s"public static void main(java.lang.String[] args)$lineSeparator{$lineSeparator    System.out.print(fibonacci(5));$lineSeparator}"
    TestGrammarUtils.compareInputWithPrint(input, None, MethodC.MethodGrammar)
  }


  def testBlock() {
    val input = "{" + lineSeparator + "    System.out.print(fibonacci(5));" + lineSeparator + "}"
    TestGrammarUtils.compareInputWithPrint(input, None, BlockC.BlockGrammar)
  }


  def testPrintAfterImplicitAddition() {
    val input = TestUtils.getJavaTestFile("fibonacci", Path("")).slurp()
    val expectation = TestUtils.getJavaTestFile("ExplicitFibonacci.java").slurp()

    val implicits = Seq[Particle](ImplicitJavaLangImport, DefaultConstructorC, ImplicitSuperConstructorCall,
      ImplicitObjectSuperClass, ConstructorC, ImplicitReturnAtEndOfMethod, ImplicitThisForPrivateMemberSelection)
    val newTransformations = JavaCompiler.spliceAfterTransformations(implicits, Seq(new PrettyPrint))

    val state = new CompilerFromParticles(newTransformations).parseAndTransform(input)
    val output = state.output

    assertResult(expectation)(output)
  }


  def testPrettyPrintByteCode() {
    val input = TestUtils.getJavaTestFile("fibonacci", Path("")).slurp()
    val expectation = TestUtils.getTestFile("FibonacciByteCodePrettyPrinted.txt").slurp()

    val prettyPrintCompiler = JavaCompiler.getPrettyPrintJavaToByteCodeCompiler

    val state = prettyPrintCompiler.parseAndTransform(input)
    assertResult(expectation)(state.output)
  }


  def testPrettyPrintAndParseByteCode() {
    val input = TestUtils.getJavaTestFile("Fibonacci.java", Path("")).slurp()

    val byteCodeTransformations = JavaCompiler.byteCodeTransformations
    val prettyPrintCompiler = JavaCompiler.getPrettyPrintJavaToByteCodeCompiler

    val state = prettyPrintCompiler.parseAndTransform(input)
    val byteCode = state.output

    val parseTransformations = Seq(RunWithJVM) ++ byteCodeTransformations
    val output = new CompilerFromParticles(parseTransformations).parseAndTransform(byteCode).output
    assertResult("8")(output)
  }


  def prettyPrintByteCode() {
    val input = TestUtils.getTestFile("FibonacciByteCodePrettyPrinted.txt").slurp()
    val parseTransformations = Seq(new PrettyPrint) ++ JavaCompiler.byteCodeTransformations
    val output = new CompilerFromParticles(parseTransformations).parseAndTransform(input).output
    assertResult(input)(output)
  }


  def parseByteCode() {
    val input = TestUtils.getTestFile("FibonacciByteCodePrettyPrinted.txt").slurp()
    val parseTransformations = JavaCompiler.byteCodeTransformations ++ Seq(RunWithJVM)
    val output = new CompilerFromParticles(parseTransformations).parseAndTransform(input).output
    assertResult("8")(output)
  }


  def testPrettyAddressLoad(): Unit = {
    val grammar = LoadAddressC.getGrammarForThisInstruction(new GrammarCatalogue())
    val addressLoad = LoadAddressC.addressLoad(0)
    BiGrammarToPrinter.toDocument(addressLoad, grammar)
  }
}
