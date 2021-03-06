package miksilo.modularLanguages.deltas.javac.statement

import miksilo.modularLanguages.util.JavaLanguageTest
import org.scalatest.funsuite.AnyFunSuite

import scala.reflect.io.Path

class TestExpressionAsStatement extends AnyFunSuite {

  test("basic") {
    val inputDirectory = Path("")
    JavaLanguageTest.compareWithJavacAfterRunning("ExpressionAsStatement", inputDirectory)
  }
}
