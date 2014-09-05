package core.grammarDocument

import org.junit.Test
import transformations.javac.expressions.TernaryC
import transformations.javac.methods.MethodC
import transformations.javac.statements.BlockC
import util.TestUtils

import scala.reflect.io.Path

class TestDocumentGrammarWithFibonacci {

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
    val input = "public static void main(java.lang.String[] args)\n{\n    System.out.print(fibonacci(5));\n}"
    TestGrammarUtils.compareInputWithPrint(input, None, MethodC.MethodGrammar)
  }

  @Test
  def testBlock() {
    val input = "{\n    System.out.print(fibonacci(5));\n}"
    TestGrammarUtils.compareInputWithPrint(input, None, BlockC.BlockGrammar)
  }
}
