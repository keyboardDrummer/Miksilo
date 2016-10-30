package transformations.javac.expressions

import core.particles.CompilerFromParticles
import org.junit.Test
import org.scalatest.FunSuite
import transformations.bytecode.additions.PoptimizeC
import transformations.javac.JavaCompiler
import util.TestUtils

class TestLong extends FunSuite {

  test("simpleLong") {
    TestUtils.compareWithJavacAfterRunning("SimpleLong")
  }

  test("longWithoutPoptimize") {
    val regularParticles = JavaCompiler.javaCompilerTransformations
    val withoutPoptimize = regularParticles.filter(p => p != PoptimizeC)
    new TestUtils(new CompilerFromParticles(withoutPoptimize)).compareWithJavacAfterRunning("SimpleLong")
  }

}
