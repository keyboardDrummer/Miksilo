package miksilo.modularLanguages.deltas.javac

import miksilo.modularLanguages.util.JavaLanguageTest
import org.scalatest.funsuite.AnyFunSuite

import scala.reflect.io.Path

class TestObjectTypeUnification extends AnyFunSuite {

  test("compareWithJavaC") {
    val inputDirectory = Path("")
    JavaLanguageTest.compareWithJavacAfterRunning("ObjectTypeUnification", inputDirectory)
  }
}
