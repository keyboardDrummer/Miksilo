package deltas.javac.classes

import org.junit.{Ignore, Test}
import org.scalatest.FunSuite
import util.LanguageTest

class TestFields extends FunSuite {

  test("FieldAssignment") {
    LanguageTest.compareWithJavacAfterRunning("FieldAssignment.java")
  }

  test("fieldMethodMix") {
    LanguageTest.compareWithJavacAfterRunning("FieldMethodMix.java")
  }

  //Don't yet feel like testing cases where Javac fails.
  ignore("illegalForwardFieldReference") {
    LanguageTest.compareWithJavacAfterRunning("IllegalForwardFieldReference.java")
  }

  //Don't yet feel like testing cases where Javac fails.
  ignore("fieldAndMethodOverloading") {
    LanguageTest.compareWithJavacAfterRunning("FieldAndMethodOverloading.java")
  }
}
