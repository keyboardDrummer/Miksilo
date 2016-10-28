package transformations.javac.expressions

import org.junit.Test
import util.TestUtils

class TestIntegers {


  def simpleInteger() {
    TestUtils.compareWithJavacAfterRunning("BigInteger")
  }
}
