package deltas.javac

import org.junit.{Assert, Test}
import org.scalatest.FunSuite
import util.TestUtils

import scala.reflect.io.Path

class TestObjectTypeUnification extends FunSuite {

  test("compareWithJavaC") {
    val inputDirectory = Path("")
    TestUtils.compareWithJavacAfterRunning("ObjectTypeUnification", inputDirectory)
  }
}
