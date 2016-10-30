package transformations.javac.statement

import org.junit.Test
import org.scalatest.FunSuite
import util.TestUtils

class TestWhile extends FunSuite {

  test("basic") {
    TestUtils.compareWithJavacAfterRunning("Whilee")
  }
}
