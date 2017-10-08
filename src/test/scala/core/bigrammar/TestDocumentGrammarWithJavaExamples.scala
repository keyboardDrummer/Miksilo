package core.bigrammar

import application.compilerCockpit._
import core.particles.Delta
import org.scalatest.FunSuite
import transformations.javac.constructor.{ConstructorC, DefaultConstructorC, ImplicitSuperConstructorCall}
import transformations.javac.expressions.ExpressionSkeleton
import transformations.javac.methods.{ImplicitReturnAtEndOfMethod, MethodDelta}
import transformations.javac.statements.BlockC
import transformations.javac.{ImplicitJavaLangImport, ImplicitObjectSuperClass, ImplicitThisForPrivateMemberSelection, JavaCompilerDeltas}
import util.{CompilerBuilder, SourceUtils}
import util.TestUtils

import scala.reflect.io.Path

class TestDocumentGrammarWithJavaExamples extends FunSuite {
  val lineSeparator = System.lineSeparator()

  test("SimpleForLoop") {
    val testFileContent = SourceUtils.getJavaTestFileContents("SimpleForLoop", Path(""))
    TestCompilerGrammarUtils.compareInputWithPrint(testFileContent, None)
  }

  test("While") {
    val testFileContent = SourceUtils.getJavaTestFileContents("Whilee", Path(""))
    TestCompilerGrammarUtils.compareInputWithPrint(testFileContent, None)
  }

  test("Fibonacci") {
    val testFileContent = SourceUtils.getJavaTestFileContents("Fibonacci", Path(""))
    TestCompilerGrammarUtils.compareInputWithPrint(testFileContent, None)
  }

  test("Ternary") {
    val input = "1 ? 2 : 3"
    TestCompilerGrammarUtils.compareInputWithPrint(input, None, ExpressionSkeleton.ExpressionGrammar)
  }

  test("FibonacciMainMethod") {
    val input = s"public static void main(java.lang.String[] args)$lineSeparator{$lineSeparator    System.out.print(fibonacci(5));$lineSeparator}"
    TestCompilerGrammarUtils.compareInputWithPrint(input, None, MethodDelta.MethodGrammar)
  }

  test("Block") {
    val input = "{" + lineSeparator + "    System.out.print(fibonacci(5));" + lineSeparator + "}"
    TestCompilerGrammarUtils.compareInputWithPrint(input, None, BlockC.BlockGrammar)
  }

  test("PrintAfterImplicitAddition") {
    val input = SourceUtils.getJavaTestFile("Fibonacci", Path(""))
    val expectation = SourceUtils.getJavaTestFileContents("ExplicitFibonacci.java")

    val implicits = Seq[Delta](ImplicitJavaLangImport, DefaultConstructorC, ImplicitSuperConstructorCall,
      ImplicitObjectSuperClass, ConstructorC, ImplicitReturnAtEndOfMethod, ImplicitThisForPrivateMemberSelection)
    val newTransformations = CompilerBuilder.build(JavaCompilerDeltas.javaCompilerTransformations).spliceAfterTransformations(implicits, Seq(new PrettyPrint))


    val state = CompilerBuilder.build(newTransformations).parseAndTransform(input)
    val output = state.output

    assertResult(expectation)(output)
  }

  test("PrettyPrintByteCode") {
    val input = SourceUtils.getJavaTestFile("Fibonacci", Path(""))
    val expectation = SourceUtils.getTestFileContents("FibonacciByteCodePrettyPrinted.txt")

    val prettyPrintCompiler = JavaCompilerDeltas.getPrettyPrintJavaToByteCodeCompiler

    val state = prettyPrintCompiler.parseAndTransform(input)
    assertResult(expectation)(state.output)
  }

  test("PrettyPrintAndParseByteCode") {
    val input = SourceUtils.getJavaTestFile("Fibonacci.java", Path(""))

    val byteCodeTransformations = JavaCompilerDeltas.byteCodeTransformations
    val prettyPrintCompiler = JavaCompilerDeltas.getPrettyPrintJavaToByteCodeCompiler

    val state = prettyPrintCompiler.parseAndTransform(input)
    val byteCode = state.output

    val parseTransformations = Seq(RunWithJVM) ++ byteCodeTransformations
    val output = CompilerBuilder.build(parseTransformations).parseAndTransform(TestUtils.stringToInputStream(byteCode)).output
    assertResult("8")(output)
  }

  test("prettyPrintByteCode") {
    val input = SourceUtils.getTestFileContents("FibonacciByteCodePrettyPrinted.txt")
    val parseTransformations = Seq(new PrettyPrint) ++ JavaCompilerDeltas.byteCodeTransformations
    val output = CompilerBuilder.build(parseTransformations).parseAndTransform(TestUtils.stringToInputStream(input)).output
    assertResult(input)(output)
  }

  test("parseByteCode") {
    val input = SourceUtils.getTestFileContents("FibonacciByteCodePrettyPrinted.txt")
    val parseTransformations = JavaCompilerDeltas.byteCodeTransformations ++ Seq(RunWithJVM)
    val output = CompilerBuilder.build(parseTransformations).parseAndTransform(TestUtils.stringToInputStream(input)).output
    assertResult("8")(output)
  }
}
