package miksilo.modularLanguages.deltas.javac.classes

import miksilo.modularLanguages.util.JavaLanguageTest
import org.scalatest.funsuite.AnyFunSuite

class TestFields extends AnyFunSuite {

  test("FieldAssignment") {
    JavaLanguageTest.compareWithJavacAfterRunning("FieldAssignment.java")
  }

  test("fieldMethodMix") {
    JavaLanguageTest.compareWithJavacAfterRunning("FieldMethodMix.java")
  }

  //Don't yet feel like testing cases where Javac fails.
  ignore("illegalForwardFieldReference") {
    JavaLanguageTest.compareWithJavacAfterRunning("IllegalForwardFieldReference.java")
  }

  //Don't yet feel like testing cases where Javac fails.
  ignore("fieldAndMethodOverloading") {
    JavaLanguageTest.compareWithJavacAfterRunning("FieldAndMethodOverloading.java")
  }
}
