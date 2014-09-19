package transformations.javaPlus

import core.transformation.CompilerFromTransformations
import org.junit.{Assert, Test}
import transformations.javac.JavaCompiler
import util.TestUtils

import scala.reflect.io.Path

class TestExpressionMethod {

  @Test
  def test() {
    val inputDirectory = Path("")
    val compiler = new CompilerFromTransformations(Seq(ExpressionMethodC) ++ JavaCompiler.javaCompilerTransformations)
    val result = new TestUtils(compiler).compileAndRun("FibonacciWithExpressionMethod", inputDirectory)
    Assert.assertEquals(8, Integer.parseInt(result))
  }
}
