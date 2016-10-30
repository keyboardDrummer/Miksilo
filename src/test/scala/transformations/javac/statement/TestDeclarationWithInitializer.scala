package transformations.javac.statement

import org.junit.Test
import org.scalatest.FunSuite
import util.TestUtils

import scala.reflect.io.Path

class TestDeclarationWithInitializer extends FunSuite {

  test("test") {
    val inputDirectory = Path("")
    TestUtils.compareWithJavacAfterRunning("DeclarationWithInitializer", inputDirectory)
  }
}
