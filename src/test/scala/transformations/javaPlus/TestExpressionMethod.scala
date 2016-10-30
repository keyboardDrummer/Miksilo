package transformations.javaPlus

import core.particles.CompilerFromParticles
import org.junit.{Assert, Test}
import org.scalatest.FunSuite
import transformations.javac.JavaCompiler
import util.TestUtils

import scala.reflect.io.Path

class TestExpressionMethod extends FunSuite {

  test("basic") {
    val inputDirectory = Path("")
    val compiler = new CompilerFromParticles(Seq(ExpressionMethodC) ++ JavaCompiler.javaCompilerTransformations)
    val result = new TestUtils(compiler).compileAndRun("FibonacciWithExpressionMethod", inputDirectory)
    assertResult(8)(Integer.parseInt(result))
  }
}
