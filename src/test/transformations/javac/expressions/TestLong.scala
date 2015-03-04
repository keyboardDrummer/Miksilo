package transformations.javac.expressions

import core.transformation.CompilerFromParticles
import org.junit.Test
import transformations.bytecode.additions.PoptimizeC
import transformations.javac.JavaCompiler
import util.TestUtils

class TestLong {

  @Test
  def simpleLong() {
    TestUtils.compareWithJavacAfterRunning("SimpleLong")
  }

  @Test
  def longWithoutPoptimize() {
    val regularParticles = JavaCompiler.javaCompilerTransformations
    val withoutPoptimize = regularParticles.filter(p => p != PoptimizeC)
    new TestUtils(new CompilerFromParticles(withoutPoptimize)).compareWithJavacAfterRunning("SimpleLong")
  }

}
