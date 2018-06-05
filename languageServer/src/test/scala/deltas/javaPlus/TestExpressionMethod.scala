package deltas.javaPlus

import org.scalatest.FunSuite
import deltas.javac.JavaLanguage
import util.TestLanguageBuilder
import util.TestUtils

import scala.reflect.io.Path

class TestExpressionMethod extends FunSuite {

  ignore("basic") {
    val inputDirectory = Path("")
    val compiler = TestLanguageBuilder.build(Seq(ExpressionMethodDelta) ++ JavaLanguage.javaCompilerDeltas)
    val result = new TestUtils(compiler).compileAndRun("FibonacciWithExpressionMethod", inputDirectory)
    assertResult(8)(Integer.parseInt(result))
  }
}
