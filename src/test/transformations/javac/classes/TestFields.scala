package transformations.javac.classes

import org.junit.Test
import util.TestUtils

class TestFields {

  @Test
  def testFieldAssignment() {
    TestUtils.compareWithJavacAfterRunning("FieldAssignment.java")
  }

  @Test
  def fieldMethodMix() {
    TestUtils.compareWithJavacAfterRunning("FieldMethodMix.java")
  }

  @Test
  def illegalForwardFieldReference() {
    TestUtils.compareWithJavacAfterRunning("IllegalForwardFieldReference.java")
  }
}
