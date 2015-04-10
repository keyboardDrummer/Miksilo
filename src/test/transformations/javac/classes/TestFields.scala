package transformations.javac.classes

import org.junit.{Ignore, Test}
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

  @Ignore //Don't yet feel like testing cases where Javac fails.
  @Test
  def illegalForwardFieldReference() {
    TestUtils.compareWithJavacAfterRunning("IllegalForwardFieldReference.java")
  }

  @Ignore //Don't yet feel like testing cases where Javac fails.
  @Test
  def fieldAndMethodOverloading() {
    TestUtils.compareWithJavacAfterRunning("FieldAndMethodOverloading.java")
  }
}
