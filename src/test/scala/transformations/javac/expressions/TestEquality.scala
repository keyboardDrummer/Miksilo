package transformations.javac.expressions

import org.junit.Test
import org.scalatest.FunSuite
import util.TestUtils

class TestEquality extends FunSuite {

  test("equality") {
    TestUtils.compareWithJavacAfterRunning("SimpleEquality")
  }
}
