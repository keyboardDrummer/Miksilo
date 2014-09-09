package core.grammarDocument

import org.junit.Test
import transformations.javac.expressions.TernaryC
import transformations.javac.methods.MethodC
import transformations.javac.statements.BlockC
import util.TestUtils

import scala.reflect.io.Path

class TestDocumentGrammarWithFibonacci {
  val lineSeparator = System.lineSeparator()

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
}
