package miksilo.modularLanguages.deltas.javac.classes

import util.JavaLanguageTest

class TestFields extends JavaLanguageTest {

  test("FieldAssignment") {
    compareWithJavacAfterRunning("FieldAssignment.java")
  }

  test("fieldMethodMix") {
    compareWithJavacAfterRunning("FieldMethodMix.java")
  }

  //Don't yet feel like testing cases where Javac fails.
  ignore("illegalForwardFieldReference") {
    compareWithJavacAfterRunning("IllegalForwardFieldReference.java")
  }

  //Don't yet feel like testing cases where Javac fails.
  ignore("fieldAndMethodOverloading") {
    compareWithJavacAfterRunning("FieldAndMethodOverloading.java")
  }
}
