package miksilo.modularLanguages.deltas.javac.statement

import miksilo.modularLanguages.util.JavaLanguageTest
import org.scalatest.funsuite.AnyFunSuite

class TestLocalDeclaration extends AnyFunSuite {

  test("IfElseBlockScoping") {
    JavaLanguageTest.compareWithJavacAfterRunning("IfElseBlockScoping.java")
  }

  test("WhileBlockScoping") {
    JavaLanguageTest.compareWithJavacAfterRunning("WhileBlockScoping.java")
  }

  ignore("IfElseWhereBothBranchesDefineSameVariableAndItIsUsedAfterwards") {
  }
}
