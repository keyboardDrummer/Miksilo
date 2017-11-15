package deltas.javac.classes

import org.junit.{Ignore, Test}
import org.scalatest.FunSuite
import util.TestUtils

class TestFields extends FunSuite {

  test("FieldAssignment") {
    TestUtils.compareWithJavacAfterRunning("FieldAssignment.java")
  }

  test("fieldMethodMix") {
    TestUtils.compareWithJavacAfterRunning("FieldMethodMix.java")
  }

  //Don't yet feel like testing cases where Javac fails.
  ignore("illegalForwardFieldReference") {
    TestUtils.compareWithJavacAfterRunning("IllegalForwardFieldReference.java")
  }

  //Don't yet feel like testing cases where Javac fails.
  ignore("fieldAndMethodOverloading") {
    TestUtils.compareWithJavacAfterRunning("FieldAndMethodOverloading.java")
  }
}
