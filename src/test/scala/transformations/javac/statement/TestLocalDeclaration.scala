package transformations.javac.statement

import org.junit.{Assert, Ignore, Test}
import org.scalatest.FunSuite
import util.TestUtils

class TestLocalDeclaration extends FunSuite {

  test("IfElseBlockScoping") {
    TestUtils.compareWithJavacAfterRunning("IfElseBlockScoping.java")
  }

  test("WhileBlockScoping") {
    TestUtils.compareWithJavacAfterRunning("WhileBlockScoping.java")
  }

  ignore("IfElseWhereBothBranchesDefineSameVariableAndItIsUsedAfterwards") {
  }
}
