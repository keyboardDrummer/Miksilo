package transformations.javac.classes

import org.junit.Test
import util.TestUtils

class TestFields {

  @Test
  def testFieldAssignment() {
    TestUtils.compareWithJavacAfterRunning("FieldAssignment")
  }

  @Test
  def fieldMethodMix() {
    TestUtils.compareWithJavacAfterRunning("FieldMethodMix")
  }
}
