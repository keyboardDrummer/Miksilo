package deltas.javac

import org.junit.Test
import util.LanguageTest

import scala.reflect.io.Path

class TestFibonacciWithMain {


  def test() {
    LanguageTest.compareWithJavacAfterRunning("Fibonacci")
  }


  def testInstanceMethod() {
    LanguageTest.compareWithJavacAfterRunning("FibonacciInstanceMethod")
  }

}
