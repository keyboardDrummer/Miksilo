package transformations.javac.expressions

import org.junit.Test
import util.TestUtils

class TestEquality {


  def test() {
    TestUtils.compareWithJavacAfterRunning("SimpleEquality")
  }
}
