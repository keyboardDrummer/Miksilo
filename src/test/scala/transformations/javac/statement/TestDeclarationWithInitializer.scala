package transformations.javac.statement

import org.junit.Test
import util.TestUtils

import scala.reflect.io.Path

class TestDeclarationWithInitializer {


  def test() {
    val inputDirectory = Path("")
    TestUtils.compareWithJavacAfterRunning("DeclarationWithInitializer", inputDirectory)
  }
}
