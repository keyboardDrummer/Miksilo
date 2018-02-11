package deltas.javac.expressions

import core.deltas.Delta
import core.language.Language
import core.smarts.SolveConstraintsDelta
import core.smarts.SolveConstraintsDelta.ConstraintException
import deltas.ClearPhases
import deltas.javac.JavaLanguage
import deltas.javac.methods.BlockLanguageDelta
import util.{TestLanguageBuilder, TestUtils}

class BlockTypeTest extends TestUtils(TestLanguageBuilder.build(
  Seq(DropPhases(1), BlockLanguageDelta) ++
    Delta.spliceAndFilterTop(
      JavaLanguage.blockWithVariables,
      JavaLanguage.javaClassSkeleton,
      Seq(SolveConstraintsDelta, ClearPhases)))) {

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

  ignore("define same variable twice") { //TODO ignore => test
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

  test("defined inside if") {
    val program =
      """int x;
        |if (true) {
        |  int y = 2;
        |  x += y;
        |}
      """.stripMargin
    compile(program)
  }

  test("defined in if, used outside it") {
    val program =
      """int x;
        |if (true) {
        |  int y = 3;
        |}
        |x += y;
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

case class DropPhases(amount: Int) extends Delta {
  override def description: String = "Drop n phases"

  override def inject(language: Language): Unit = {
    language.compilerPhases = language.compilerPhases.drop(amount)
  }
}
