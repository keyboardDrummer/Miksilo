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
    JavaCompilerDeltas.javaSimpleStatement)) {

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
