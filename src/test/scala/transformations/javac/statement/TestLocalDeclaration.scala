package transformations.javac.statement

import org.junit.{Ignore, Assert, Test}
import util.TestUtils

class TestLocalDeclaration {

  @Test
  def testIfElseBlockScoping() {
    TestUtils.compareWithJavacAfterRunning("IfElseBlockScoping.java")
  }

  @Test
  def testWhileBlockScoping() {
    TestUtils.compareWithJavacAfterRunning("WhileBlockScoping.java")
  }

  @Ignore
  @Test
  def testIfElseWhereBothBranchesDefineSameVariableAndItIsUsedAfterwards() = {
    Assert.fail()
  }
}
