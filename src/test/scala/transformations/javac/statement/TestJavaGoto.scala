package transformations.javac.statement

import core.particles.CompilerFromParticles
import org.junit.{Assert, Test}
import org.scalatest.FunSuite
import transformations.javac.JavaCompiler
import transformations.javac.statements.JavaGotoC
import util.TestUtils

class TestJavaGoto extends FunSuite {


  def test() {
    val testUtils: TestUtils = new TestUtils(new CompilerFromParticles(Seq(JavaGotoC) ++ JavaCompiler.javaCompilerTransformations))
    assertResult("11")(testUtils.compileAndRun("JavaGoto"))
  }
}
