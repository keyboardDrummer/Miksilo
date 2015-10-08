package transformations.javac

import org.junit.Test
import util.TestUtils

import scala.reflect.io.Path

class TestFibonacciWithMain {

  @Test
  def test() {
    TestUtils.compareWithJavacAfterRunning("Fibonacci")
  }

  @Test
  def testInstanceMethod() {
    TestUtils.compareWithJavacAfterRunning("FibonacciInstanceMethod")
  }

}
