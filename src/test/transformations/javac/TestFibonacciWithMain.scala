package transformations.javac

import org.junit.Test
import util.TestUtils

import scala.reflect.io.Path

class TestFibonacciWithMain {

  @Test
  def test() {
    val inputDirectory = Path("")
    TestUtils.compareWithJavacAfterRunning("Fibonacci", inputDirectory)
  }

  @Test
  def testInstanceMethod() {
    val inputDirectory = Path("")
    TestUtils.compareWithJavacAfterRunning("FibonacciInstanceMethod", inputDirectory)
  }

}
