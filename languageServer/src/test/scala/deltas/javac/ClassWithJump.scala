package deltas.javac

import org.scalatest.FunSuite
import util.LanguageTest

import scala.reflect.io.Path

class ClassWithJump extends FunSuite {

  test("basic") {
    val inputDirectory = Path("")
    LanguageTest.compareWithJavacAfterRunning("ClassWithJump", inputDirectory)
  }
}
