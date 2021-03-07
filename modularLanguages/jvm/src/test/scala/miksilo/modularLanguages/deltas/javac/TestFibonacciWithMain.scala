package miksilo.modularLanguages.deltas.javac

import miksilo.modularLanguages.util.JavaLanguageTest
import org.scalatest.funsuite.AnyFunSuite

class TestFibonacciWithMain extends AnyFunSuite {

  def test(): Unit = {
    JavaLanguageTest.compareWithJavacAfterRunning("Fibonacci")
  }

  def testInstanceMethod(): Unit = {
    JavaLanguageTest.compareWithJavacAfterRunning("FibonacciInstanceMethod")
  }

}
