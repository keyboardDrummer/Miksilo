package core.bigrammar

import application.compilerCockpit._
import core.bigrammar.printer.BiGrammarToPrinter
import core.particles.grammars.GrammarCatalogue
import core.particles.{CompilerFromParticles, Particle}
import org.scalatest.{FunSpec, FunSuite}
import transformations.bytecode.coreInstructions.objects.LoadAddressC
import transformations.javac.constructor.{ConstructorC, DefaultConstructorC, ImplicitSuperConstructorCall}
import transformations.javac.expressions.{ExpressionSkeleton, TernaryC}
import transformations.javac.methods.{ImplicitReturnAtEndOfMethod, MethodC}
import transformations.javac.statements.BlockC
import transformations.javac.{ImplicitJavaLangImport, ImplicitObjectSuperClass, ImplicitThisForPrivateMemberSelection, JavaCompiler}
import util.TestUtils

import scala.reflect.io.Path

class TestDocumentGrammarWithJavaExamples extends FunSuite {
  val lineSeparator = System.lineSeparator()

  test("SimpleForLoop") {
    val testFileContent = TestUtils.getJavaTestFile("SimpleForLoop", Path("")).slurp()
    TestGrammarUtils.compareInputWithPrint(testFileContent, None)
  }

  test("While") {
    val testFileContent = TestUtils.getJavaTestFile("Whilee", Path("")).slurp()
    TestGrammarUtils.compareInputWithPrint(testFileContent, None)
  }

  test("Fibonacci") {
    val testFileContent = TestUtils.getJavaTestFile("Fibonacci", Path("")).slurp()
    TestGrammarUtils.compareInputWithPrint(testFileContent, None)
  }

  test("Ternary") {
    val input = "1 ? 2 : 3"
    TestGrammarUtils.compareInputWithPrint(input, None, ExpressionSkeleton.ExpressionGrammar)
  }

  test("FibonacciMainMethod") {
    val input = s"public static void main(java.lang.String[] args)$lineSeparator{$lineSeparator    System.out.print(fibonacci(5));$lineSeparator}"
    TestGrammarUtils.compareInputWithPrint(input, None, MethodC.MethodGrammar)
  }

  test("Block") {
    val input = "{" + lineSeparator + "    System.out.print(fibonacci(5));" + lineSeparator + "}"
    TestGrammarUtils.compareInputWithPrint(input, None, BlockC.BlockGrammar)
  }

  test("PrintAfterImplicitAddition") {
    val input = TestUtils.getJavaTestFile("Fibonacci", Path("")).slurp()
    val expectation = TestUtils.getJavaTestFile("ExplicitFibonacci.java").slurp()

    val implicits = Seq[Particle](ImplicitJavaLangImport, DefaultConstructorC, ImplicitSuperConstructorCall,
      ImplicitObjectSuperClass, ConstructorC, ImplicitReturnAtEndOfMethod, ImplicitThisForPrivateMemberSelection)
    val newTransformations = JavaCompiler.spliceAfterTransformations(implicits, Seq(new PrettyPrint))

    val state = new CompilerFromParticles(newTransformations).parseAndTransform(input)
    val output = state.output

    assertResult(expectation)(output)
  }

  test("PrettyPrintByteCode") {
    val input = TestUtils.getJavaTestFile("Fibonacci", Path("")).slurp()
    val expectation = TestUtils.getTestFile("FibonacciByteCodePrettyPrinted.txt").slurp()

    val prettyPrintCompiler = JavaCompiler.getPrettyPrintJavaToByteCodeCompiler

    val state = prettyPrintCompiler.parseAndTransform(input)
    assertResult(expectation)(state.output)
  }

  test("PrettyPrintAndParseByteCode") {
    val input = TestUtils.getJavaTestFile("Fibonacci.java", Path("")).slurp()

    val byteCodeTransformations = JavaCompiler.byteCodeTransformations
    val prettyPrintCompiler = JavaCompiler.getPrettyPrintJavaToByteCodeCompiler

    val state = prettyPrintCompiler.parseAndTransform(input)
    val byteCode = state.output

    val parseTransformations = Seq(RunWithJVM) ++ byteCodeTransformations
    val output = new CompilerFromParticles(parseTransformations).parseAndTransform(byteCode).output
    assertResult("8")(output)
  }

  test("prettyPrintByteCode") {
    val input = TestUtils.getTestFile("FibonacciByteCodePrettyPrinted.txt").slurp()
    val parseTransformations = Seq(new PrettyPrint) ++ JavaCompiler.byteCodeTransformations
    val output = new CompilerFromParticles(parseTransformations).parseAndTransform(input).output
    assertResult(input)(output)
  }

  test("parseByteCode") {
    val input = TestUtils.getTestFile("FibonacciByteCodePrettyPrinted.txt").slurp()
    val parseTransformations = JavaCompiler.byteCodeTransformations ++ Seq(RunWithJVM)
    val output = new CompilerFromParticles(parseTransformations).parseAndTransform(input).output
    assertResult("8")(output)
  }

  test("PrettyAddressLoad") {
    val grammar = LoadAddressC.getGrammarForThisInstruction(new GrammarCatalogue())
    val addressLoad = LoadAddressC.addressLoad(0)
    BiGrammarToPrinter.toDocument(addressLoad, grammar)
  }
}
