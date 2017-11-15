package deltas.javac

import org.junit.Test
import util.TestUtils

import scala.reflect.io.Path

class TestFibonacciWithMain {


  def test() {
    TestUtils.compareWithJavacAfterRunning("Fibonacci")
  }


  def testInstanceMethod() {
    TestUtils.compareWithJavacAfterRunning("FibonacciInstanceMethod")
  }

}
