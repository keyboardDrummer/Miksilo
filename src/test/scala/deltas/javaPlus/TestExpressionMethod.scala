package deltas.javaPlus

import org.scalatest.FunSuite
import deltas.javac.JavaCompilerDeltas
import util.TestLanguageBuilder
import util.TestUtils

import scala.reflect.io.Path

class TestExpressionMethod extends FunSuite {

  test("basic") {
    val inputDirectory = Path("")
    val compiler = TestLanguageBuilder.build(Seq(ExpressionMethodDelta) ++ JavaCompilerDeltas.javaCompilerDeltas)
    val result = new TestUtils(compiler).compileAndRun("FibonacciWithExpressionMethod", inputDirectory)
    assertResult(8)(Integer.parseInt(result))
  }
}
