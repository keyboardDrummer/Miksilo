package deltas.javac

import util.JavaLanguageTest

class TestFibonacciWithMain extends JavaLanguageTest {

  def test(): Unit = {
    compareWithJavacAfterRunning("Fibonacci")
  }

  def testInstanceMethod(): Unit = {
    compareWithJavacAfterRunning("FibonacciInstanceMethod")
  }

}
