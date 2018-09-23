package deltas.javac

import util.JavaLanguageTest

import scala.reflect.io.Path

class ClassWithJump extends JavaLanguageTest {

  test("basic") {
    val inputDirectory = Path("")
    compareWithJavacAfterRunning("ClassWithJump", inputDirectory)
  }
}
