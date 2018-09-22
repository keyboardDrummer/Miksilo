package deltas.javac.statement

import org.junit.{Assert, Ignore, Test}
import org.scalatest.FunSuite
import util.LanguageTest

class TestLocalDeclaration extends FunSuite {

  test("IfElseBlockScoping") {
    LanguageTest.compareWithJavacAfterRunning("IfElseBlockScoping.java")
  }

  test("WhileBlockScoping") {
    LanguageTest.compareWithJavacAfterRunning("WhileBlockScoping.java")
  }

  ignore("IfElseWhereBothBranchesDefineSameVariableAndItIsUsedAfterwards") {
  }
}
