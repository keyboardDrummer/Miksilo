package deltas.javac.expressions

import core.nabl.SolveConstraintsDelta
import core.nabl.SolveConstraintsDelta.ConstraintException
import deltas.ClearPhases
import deltas.javac.JavaCompilerDeltas
import deltas.javac.methods.BlockLanguageDelta
import util.{TestLanguageBuilder, TestUtils}

class BlockTypeTest extends TestUtils(TestLanguageBuilder.build(
  Seq(SolveConstraintsDelta,
    BlockLanguageDelta,
    ClearPhases) ++
    JavaCompilerDeltas.methodBlock)) {

  test("int variable") {
    val program =
      """int x;
        |x = 3;
      """.stripMargin
    compile(program)
  }

  test("assign long to int variable") {
    val program =
      """int x;
        |x = 3l;
      """.stripMargin
    assertThrows[ConstraintException](compile(program))
  }

  test("define same variable twice") {
    val program =
      """int x;
        |int x;
      """.stripMargin
    assertThrows[ConstraintException](compile(program))
  }

  test("use variable that does not exist") {
    val program =
      """int x;
        |y = 3;
      """.stripMargin
    assertThrows[ConstraintException](compile(program))
  }

  test("int + int") {
    val program = "3 + 2;"
    compile(program)
  }

  test("int + long") {
    val program = "3 + 2l;"
    assertThrows[ConstraintException](compile(program))
  }

  test("long + long") {
    val program = "3l + 2l;"
    compile(program)
  }
}
