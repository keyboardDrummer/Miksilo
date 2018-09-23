package deltas.javac

import util.JavaLanguageTest

import scala.reflect.io.Path

class TestObjectTypeUnification extends JavaLanguageTest {

  test("compareWithJavaC") {
    val inputDirectory = Path("")
    compareWithJavacAfterRunning("ObjectTypeUnification", inputDirectory)
  }
}
