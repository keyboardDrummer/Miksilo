package miksilo.modularLanguages.deltas.javac

import miksilo.modularLanguages.util.JavaLanguageTest

import scala.reflect.io.Path

class ClassWithJump extends JavaLanguageTest {

  test("basic") {
    val inputDirectory = Path("")
    compareWithJavacAfterRunning("ClassWithJump", inputDirectory)
  }
}
