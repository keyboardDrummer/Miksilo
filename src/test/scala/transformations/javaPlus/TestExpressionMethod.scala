package transformations.javaPlus

import org.scalatest.FunSuite
import transformations.javac.JavaCompiler
import util.{CompilerBuilder, TestUtils}

import scala.reflect.io.Path

class TestExpressionMethod extends FunSuite {

  test("basic") {
    val inputDirectory = Path("")
    val compiler = CompilerBuilder.build(Seq(ExpressionMethodC) ++ JavaCompiler.javaCompilerTransformations)
    val result = new TestUtils(compiler).compileAndRun("FibonacciWithExpressionMethod", inputDirectory)
    assertResult(8)(Integer.parseInt(result))
  }
}
