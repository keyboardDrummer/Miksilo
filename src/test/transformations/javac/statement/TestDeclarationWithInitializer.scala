package transformations.javac.statement

import org.junit.Test
import transformations.javac.TestUtils

import scala.reflect.io.Path

class TestDeclarationWithInitializer {

  @Test
  def test() {
    val inputDirectory = Path("")
    TestUtils.compareWithJavacAfterRunning("DeclarationWithInitializer", inputDirectory)
  }
}
