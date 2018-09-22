package deltas.javac.expressions

import org.scalatest.FunSuite
import util.LanguageTest

import scala.reflect.io.Path

class TestPostfixIncrement extends FunSuite {

  test("basic") {
    val inputDirectory = Path("")
    LanguageTest.compareWithJavacAfterRunning("PostFixIncrement", inputDirectory)
  }
}
