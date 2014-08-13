package transformations.javac

import org.junit.Test

import scala.reflect.io.Path

class ClassWithJump {

  @Test
  def test() {
    val inputDirectory = Path("")
    TestUtils.compareWithJavacAfterRunning("ClassWithJump", inputDirectory)
  }
}
