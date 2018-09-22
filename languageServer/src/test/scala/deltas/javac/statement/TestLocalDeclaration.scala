package deltas.javac.statement

import util.JavaLanguageTest

class TestLocalDeclaration extends JavaLanguageTest {

  test("IfElseBlockScoping") {
    compareWithJavacAfterRunning("IfElseBlockScoping.java")
  }

  test("WhileBlockScoping") {
    compareWithJavacAfterRunning("WhileBlockScoping.java")
  }

  ignore("IfElseWhereBothBranchesDefineSameVariableAndItIsUsedAfterwards") {
  }
}
