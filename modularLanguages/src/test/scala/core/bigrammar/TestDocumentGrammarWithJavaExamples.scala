package core.bigrammar

import core.SolveConstraintsDelta
import core.deltas.Delta
import deltas.expression.ExpressionDelta
import deltas.javac.ByteCodeLanguage.byteCodeDeltas
import deltas.javac.JavaToByteCodeLanguage.spliceBeforeTransformations
import deltas.javac._
import deltas.javac.constructor.{ConstructorDelta, DefaultConstructorDelta, ImplicitSuperConstructorCall}
import deltas.javac.methods.{ImplicitReturnAtEndOfMethod, MethodDelta}
import deltas.statement.BlockDelta
import deltas.{PrettyPrint, RunWithJVM}
import org.scalatest.FunSuite
import util.{JavaSourceUtils, TestLanguageBuilder}

import scala.reflect.io.Path

class TestDocumentGrammarWithJavaExamples extends FunSuite {
  val lineSeparator: String = System.lineSeparator()

  test("SimpleForLoop") {
    val testFileContent = JavaSourceUtils.getJavaTestFileContents("SimpleForLoop", Path(""))
    TestLanguageGrammarUtils.compareInputWithPrint(testFileContent, None)
  }

  test("While") {
    val testFileContent = JavaSourceUtils.getJavaTestFileContents("Whilee", Path(""))
    TestLanguageGrammarUtils.compareInputWithPrint(testFileContent, None)
  }

  test("Fibonacci") {
    val testFileContent = JavaSourceUtils.getJavaTestFileContents("Fibonacci", Path(""))
    TestLanguageGrammarUtils.compareInputWithPrint(testFileContent, None)
  }

  test("Ternary") {
    val input = "1 ? 2 : 3"
    TestLanguageGrammarUtils.compareInputWithPrint(input, None, ExpressionDelta.FirstPrecedenceGrammar)
  }

  test("SystemPrintX") {
    val input = s"System.print(x)"
    TestLanguageGrammarUtils.compareInputWithPrint(input, None, ExpressionDelta.FirstPrecedenceGrammar)
  }

  /*
  Deze case is lastig omdat Selector.Member eerst print is, maar daarna out wordt,
  en daarna verdwijnt voordat print gehandled wordt.

  Hoe werkt dit tijdens parsen?
   */
  test("SystemOutPrintX") {
    val input = s"System.out.print(x)"
    TestLanguageGrammarUtils.compareInputWithPrint(input, None, ExpressionDelta.FirstPrecedenceGrammar)
  }

  test("FibonacciMainMethod") {
    val input = s"public static void main(java.lang.String[] args)$lineSeparator{$lineSeparator    System.out.print(fibonacci(5));$lineSeparator}"
    TestLanguageGrammarUtils.compareInputWithPrint(input, None, MethodDelta.Shape)
  }

  test("Block") {
    val input = "{" + lineSeparator + "    System.out.print(fibonacci(5));" + lineSeparator + "}"
    TestLanguageGrammarUtils.compareInputWithPrint(input, None, BlockDelta.BlockGrammar)
  }

  test("PrintAfterImplicitAddition") {
    val input = JavaSourceUtils.getJavaTestFile("Fibonacci", Path(""))
    val expectation = JavaSourceUtils.getJavaTestFileContents("ExplicitFibonacci.java")

    val implicits = Seq[Delta](ImplicitJavaLangImport, DefaultConstructorDelta, ImplicitSuperConstructorCall,
      ImplicitObjectSuperClass, ConstructorDelta, ImplicitReturnAtEndOfMethod, SolveConstraintsDelta, ImplicitThisForPrivateMemberSelectionDelta)
    val newDeltas = TestLanguageBuilder.buildWithParser(JavaToByteCodeLanguage.javaCompilerDeltas).spliceAfterTransformations(implicits, Seq(new PrettyPrint))

    val state = TestLanguageBuilder.buildWithParser(newDeltas).compileStream(input)
    val output = state.output

    assertResult(expectation)(output)
  }

  test("PrettyPrintByteCode") {
    val input = JavaSourceUtils.getJavaTestFile("Fibonacci", Path(""))
    val expectation = JavaSourceUtils.getTestFileContents("FibonacciByteCodePrettyPrinted.txt")

    val prettyPrintCompiler = getPrettyPrintJavaToByteCodeCompiler

    val state = prettyPrintCompiler.compileStream(input)
    assertResult(expectation)(state.output)
  }

  def getPrettyPrintJavaToByteCodeCompiler = {
    TestLanguageBuilder.build(spliceBeforeTransformations(byteCodeDeltas, Seq(new PrettyPrint)))
  }

  test("PrettyPrintAndParseByteCode") {
    val input = JavaSourceUtils.getJavaTestFile("Fibonacci.java", Path(""))

    val byteCodeTransformations = ByteCodeLanguage.byteCodeDeltas
    val prettyPrintCompiler = getPrettyPrintJavaToByteCodeCompiler

    val state = prettyPrintCompiler.compileStream(input)
    val byteCode = state.output

    val parseTransformations = Seq(RunWithJVM) ++ byteCodeTransformations
    val output = TestLanguageBuilder.buildWithParser(parseTransformations).compileStream(JavaSourceUtils.stringToStream(byteCode)).output
    assertResult("8")(output)
  }

  test("prettyPrintByteCode") {
    val input = JavaSourceUtils.getTestFileContents("FibonacciByteCodePrettyPrinted.txt")
    val parseTransformations = Seq(new PrettyPrint) ++ ByteCodeLanguage.byteCodeDeltas
    val output = TestLanguageBuilder.buildWithParser(parseTransformations).compileStream(JavaSourceUtils.stringToStream(input)).output
    assertResult(input)(output)
  }

  test("parseByteCode") {
    val input = JavaSourceUtils.getTestFileContents("FibonacciByteCodePrettyPrinted.txt")
    val deltas = Seq(RunWithJVM) ++ ByteCodeLanguage.byteCodeDeltas
    val output = TestLanguageBuilder.buildWithParser(deltas).compileStream(JavaSourceUtils.stringToStream(input)).output
    assertResult("8")(output)
  }
}
