package transformations.javac

import org.junit.Test

import scala.reflect.io.Path

class TestFibonacciWithMain {

  @Test
  def test() {
    val inputDirectory = Path("")
    TestUtils.compareWithJavacAfterRunning("Fibonacci", inputDirectory)
  }

}
