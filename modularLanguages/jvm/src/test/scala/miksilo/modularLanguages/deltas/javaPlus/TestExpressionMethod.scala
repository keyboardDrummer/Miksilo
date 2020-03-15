package miksilo.modularLanguages.deltas.javaPlus

import miksilo.modularLanguages.deltas.javac.JavaToByteCodeLanguage
import miksilo.modularLanguages.util.{LanguageTest, TestLanguageBuilder}
import org.scalatest.funsuite.AnyFunSuite

import scala.reflect.io.Path

class TestExpressionMethod extends AnyFunSuite {

  test("basic") {
    val inputDirectory = Path("")
    val compiler = TestLanguageBuilder.buildWithParser(Seq(ExpressionMethodDelta) ++ JavaToByteCodeLanguage.javaCompilerDeltas)
    val result = new LanguageTest(compiler).compileAndRun("FibonacciWithExpressionMethod", inputDirectory)
    assertResult(8)(Integer.parseInt(result))
  }
}
