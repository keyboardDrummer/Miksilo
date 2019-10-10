package deltas.javac

import util.JavaLanguageTest

class TestFibonacciWithMain extends JavaLanguageTest {

  def test() {
    compareWithJavacAfterRunning("Fibonacci")
  }

  def testInstanceMethod() {
    compareWithJavacAfterRunning("FibonacciInstanceMethod")
  }

}
