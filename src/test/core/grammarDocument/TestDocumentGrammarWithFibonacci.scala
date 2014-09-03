package core.grammarDocument

import org.junit.Test
import transformations.javac.expressions.TernaryC
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
}
