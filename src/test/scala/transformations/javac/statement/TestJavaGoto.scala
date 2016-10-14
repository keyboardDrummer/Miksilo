package transformations.javac.statement

import core.particles.CompilerFromParticles
import org.junit.{Assert, Test}
import transformations.javac.JavaCompiler
import transformations.javac.statements.JavaGotoC
import util.TestUtils

class TestJavaGoto {

  @Test
  def test() {
    val testUtils: TestUtils = new TestUtils(new CompilerFromParticles(Seq(JavaGotoC) ++ JavaCompiler.javaCompilerTransformations))
    Assert.assertEquals("11",testUtils.compileAndRun("JavaGoto"))
  }
}
