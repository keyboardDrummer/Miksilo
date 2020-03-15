package miksilo.modularLanguages.deltas.javac

import miksilo.modularLanguagesutil.JavaLanguageTest

import scala.reflect.io.Path

class TestObjectTypeUnification extends JavaLanguageTest {

  test("compareWithJavaC") {
    val inputDirectory = Path("")
    compareWithJavacAfterRunning("ObjectTypeUnification", inputDirectory)
  }
}
