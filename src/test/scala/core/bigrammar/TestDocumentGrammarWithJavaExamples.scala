package core.bigrammar

import application.compilerCockpit._
import core.deltas.Delta
import org.scalatest.FunSuite
import deltas.javac.constructor.{ConstructorDelta, DefaultConstructorDelta, ImplicitSuperConstructorCall}
import deltas.javac.expressions.ExpressionSkeleton
import deltas.javac.methods.{ImplicitReturnAtEndOfMethod, MethodDelta}
import deltas.javac.statements.BlockDelta
import deltas.javac.{ImplicitJavaLangImport, ImplicitObjectSuperClass, ImplicitThisForPrivateMemberSelection, JavaLanguage}
import util.{TestLanguageBuilder, SourceUtils}
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

  test("SystemPrintX") {
    val input = s"System.print(x)"
    TestCompilerGrammarUtils.compareInputWithPrint(input, None, ExpressionSkeleton.ExpressionGrammar)
  }

  /*
  Deze case is lastig omdat Selector.Member eerst print is, maar daarna out wordt,
  en daarna verdwijnt voordat print gehandled wordt.

  Hoe werkt dit tijdens parsen?
   */
  test("SystemOutPrintX") {
    val input = s"System.out.print(x)"
    TestCompilerGrammarUtils.compareInputWithPrint(input, None, ExpressionSkeleton.ExpressionGrammar)
  }

  test("FibonacciMainMethod") {
    val input = s"public static void main(java.lang.String[] args)$lineSeparator{$lineSeparator    System.out.print(fibonacci(5));$lineSeparator}"
    TestCompilerGrammarUtils.compareInputWithPrint(input, None, MethodDelta.MethodGrammar)
  }

  test("Block") {
    val input = "{" + lineSeparator + "    System.out.print(fibonacci(5));" + lineSeparator + "}"
    TestCompilerGrammarUtils.compareInputWithPrint(input, None, BlockDelta.Grammar)
  }

  test("PrintAfterImplicitAddition") {
    val input = SourceUtils.getJavaTestFile("Fibonacci", Path(""))
    val expectation = SourceUtils.getJavaTestFileContents("ExplicitFibonacci.java")

    val implicits = Seq[Delta](ImplicitJavaLangImport, DefaultConstructorDelta, ImplicitSuperConstructorCall,
      ImplicitObjectSuperClass, ConstructorDelta, ImplicitReturnAtEndOfMethod, ImplicitThisForPrivateMemberSelection)
    val newTransformations = TestLanguageBuilder.build(JavaLanguage.javaCompilerDeltas).spliceAfterTransformations(implicits, Seq(new PrettyPrint))

    val state = TestLanguageBuilder.build(newTransformations).parseAndTransform(input)
    val output = state.output

    assertResult(expectation)(output)
  }

  test("PrettyPrintByteCode") {
    val input = SourceUtils.getJavaTestFile("Fibonacci", Path(""))
    val expectation = SourceUtils.getTestFileContents("FibonacciByteCodePrettyPrinted.txt")

    val prettyPrintCompiler = JavaLanguage.getPrettyPrintJavaToByteCodeCompiler

    val state = prettyPrintCompiler.parseAndTransform(input)
    assertResult(expectation)(state.output)
  }

  test("PrettyPrintAndParseByteCode") {
    val input = SourceUtils.getJavaTestFile("Fibonacci.java", Path(""))

    val byteCodeTransformations = JavaLanguage.byteCodeDeltas
    val prettyPrintCompiler = JavaLanguage.getPrettyPrintJavaToByteCodeCompiler

    val state = prettyPrintCompiler.parseAndTransform(input)
    val byteCode = state.output

    val parseTransformations = Seq(RunWithJVM) ++ byteCodeTransformations
    val output = TestLanguageBuilder.build(parseTransformations).parseAndTransform(TestUtils.stringToInputStream(byteCode)).output
    assertResult("8")(output)
  }

  test("prettyPrintByteCode") {
    val input = SourceUtils.getTestFileContents("FibonacciByteCodePrettyPrinted.txt")
    val parseTransformations = Seq(new PrettyPrint) ++ JavaLanguage.byteCodeDeltas
    val output = TestLanguageBuilder.build(parseTransformations).parseAndTransform(TestUtils.stringToInputStream(input)).output
    assertResult(input)(output)
  }

  test("parseByteCode") {
    val input = SourceUtils.getTestFileContents("FibonacciByteCodePrettyPrinted.txt")
    val parseTransformations = Seq(RunWithJVM) ++ JavaLanguage.byteCodeDeltas
    val output = TestLanguageBuilder.build(parseTransformations).parseAndTransform(TestUtils.stringToInputStream(input)).output
    assertResult("8")(output)
  }
}
