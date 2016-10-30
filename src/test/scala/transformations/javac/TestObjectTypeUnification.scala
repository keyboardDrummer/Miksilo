package transformations.javac

import org.junit.{Assert, Test}
import org.scalatest.FunSuite
import util.TestUtils

import scala.reflect.io.Path

class TestObjectTypeUnification extends FunSuite {

  test("FullPipeline") {
    val inputDirectory = Path("")
    val output: String = TestUtils.compileAndRun("ObjectTypeUnification", inputDirectory)
    assertResult("3")( output)
  }

  test("compareWithJavaC") {
    val inputDirectory = Path("")
    TestUtils.compareWithJavacAfterRunning("ObjectTypeUnification", inputDirectory)
  }
}
