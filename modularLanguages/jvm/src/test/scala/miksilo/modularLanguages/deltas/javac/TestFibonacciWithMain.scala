package miksilo.modularLanguages.deltas.javac

import miksilo.modularLanguagesutil.JavaLanguageTest

class TestFibonacciWithMain extends JavaLanguageTest {

  def test(): Unit = {
    compareWithJavacAfterRunning("Fibonacci")
  }

  def testInstanceMethod(): Unit = {
    compareWithJavacAfterRunning("FibonacciInstanceMethod")
  }

}
