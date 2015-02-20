package transformations.javac.expressions

import core.transformation.CompilerFromParticles
import org.junit.Test
import transformations.bytecode.additions.PoptimizeC
import transformations.javac.JavaCompiler
import util.TestUtils

import scala.reflect.io.Path

class TestLong {

  @Test
  def simpleLong() {
    val inputDirectory = Path("")
    TestUtils.compareWithJavacAfterRunning("SimpleLong", inputDirectory)
  }

  @Test
  def longWithoutPoptimize() {
    val inputDirectory = Path("")
    val regularParticles = JavaCompiler.javaCompilerTransformations
    val withoutPoptimize = regularParticles.filter(p => p != PoptimizeC)
    new TestUtils(new CompilerFromParticles(withoutPoptimize)).compareWithJavacAfterRunning("SimpleLong", inputDirectory)
  }

}
