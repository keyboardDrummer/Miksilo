package transformations.javac.statement

import org.junit.Test
import util.TestUtils

class TestLocalDeclaration {

  @Test
  def testIfElseBlockScoping() {
    TestUtils.compareWithJavacAfterRunning("IfElseBlockScoping")
  }

  @Test
  def testWhileBlockScoping() {
    TestUtils.compareWithJavacAfterRunning("WhileBlockScoping")
  }
}
