package miksilo.modularLanguages.deltas.javac

import miksilo.modularLanguages.util.JavaLanguageTest
import org.scalatest.funsuite.AnyFunSuite

import scala.reflect.io.Path

class ClassWithJump extends AnyFunSuite {

  test("basic") {
    val inputDirectory = Path("")
    JavaLanguageTest.compareWithJavacAfterRunning("ClassWithJump", inputDirectory)
  }
}
