package transformations.javac.expressions

import org.junit.Test
import org.scalatest.FunSuite
import util.TestUtils

class TestIntegers extends FunSuite {

  test("simpleInteger") {
    TestUtils.compareWithJavacAfterRunning("BigInteger")
  }
}
