package deltas.javac.expressions

import core.language.Language
import core.nabl.SolveConstraintsDelta
import deltas.ClearPhases
import deltas.javac.JavaCompilerDeltas
import util.{SourceUtils, TestLanguageBuilder, TestUtils}

class JavaClassTypeTest extends TestUtils(TestLanguageBuilder.build(
    Language.spliceAndFilterTop(
        JavaCompilerDeltas.javaCompilerDeltas,
        JavaCompilerDeltas.javaClassSkeleton,
        Seq(SolveConstraintsDelta, ClearPhases)))) {

  test("empty class") {
    val program =
      """class Test { }""".stripMargin
    compile(program)
  }

  test("empty main method") {
    val program =
      """class Test {
        |  public static void main(String[] args) {
        |  }
        |}""".stripMargin
    compile(program)
  }

  test("fibonacci") {
    val program = SourceUtils.getJavaTestFileContents("Fibonacci")
    compile(program)
  }
}
