package deltas.javac.expressions

import core.deltas.Delta
import core.smarts.SolveConstraintsDelta
import deltas.ClearPhases
import deltas.javac.{ImplicitThisForPrivateMemberSelectionDelta, JavaToByteCodeLanguage}
import deltas.javac.methods.BlockLanguageDelta
import util.{LanguageTest, TestLanguageBuilder}

class BlockTypeTest extends LanguageTest(TestLanguageBuilder.buildWithParser(
  Seq(DropPhases(1), BlockLanguageDelta) ++
    Delta.spliceAndFilterTop(
      JavaToByteCodeLanguage.blockWithVariables,
      JavaToByteCodeLanguage.javaClassSkeleton,
      Seq(SolveConstraintsDelta, ClearPhases)).filter(d => d != ImplicitThisForPrivateMemberSelectionDelta))) {

  test("int variable") {
    val program =
      """int x;
        |x = 3;
      """.stripMargin
    val compilation = compile(program)
    assert(compilation.remainingConstraints.isEmpty)
  }

  test("assign long to int variable") {
    val program =
      """int x;
        |x = 3l;
      """.stripMargin
    val compilation = compile(program)
    assert(compilation.remainingConstraints.nonEmpty)
  }

  ignore("define same variable twice") { //TODO ignore => test
    val program =
      """int x;
        |int x;
      """.stripMargin
    val compilation = compile(program)
    assert(compilation.remainingConstraints.nonEmpty)
  }

  test("use variable that does not exist") {
    val program =
      """int x;
        |y = 3;
      """.stripMargin
    val compilation = compile(program)
    assert(compilation.remainingConstraints.nonEmpty)
  }

  test("defined inside if") {
    val program =
      """int x;
        |if (true) {
        |  int y = 2;
        |  x += y;
        |}
      """.stripMargin
    val compilation = compile(program)
    assert(compilation.remainingConstraints.isEmpty)
  }

  test("defined in if, used outside it") {
    val program =
      """int x;
        |if (true) {
        |  int y = 3;
        |}
        |x += y;
      """.stripMargin
    val compilation = compile(program)
    assert(compilation.remainingConstraints.nonEmpty)
  }

  test("int + int") {
    val program = "3 + 2;"
    compile(program)
  }

  test("int + long") {
    val program = "3 + 2l;"
    val compilation = compile(program)
    assert(compilation.remainingConstraints.nonEmpty)
  }

  test("long + long") {
    val program = "3l + 2l;"
    val compilation = compile(program)
    assert(compilation.remainingConstraints.isEmpty)
  }
}


