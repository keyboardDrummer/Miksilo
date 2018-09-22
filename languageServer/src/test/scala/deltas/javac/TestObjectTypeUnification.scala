package deltas.javac

import org.junit.{Assert, Test}
import org.scalatest.FunSuite
import util.LanguageTest

import scala.reflect.io.Path

class TestObjectTypeUnification extends FunSuite {

  test("compareWithJavaC") {
    val inputDirectory = Path("")
    LanguageTest.compareWithJavacAfterRunning("ObjectTypeUnification", inputDirectory)
  }
}
