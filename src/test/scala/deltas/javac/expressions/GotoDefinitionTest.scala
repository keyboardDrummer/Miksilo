package deltas.javac.expressions

import core.deltas.Delta
import core.language.LanguageServer
import core.language.node.{Position, SourceRange}
import core.smarts.SolveConstraintsDelta
import deltas.ClearPhases
import deltas.javac.JavaLanguage
import deltas.javac.methods.BlockLanguageDelta
import org.scalatest.FunSuite
import util.SourceUtils

class GotoDefinitionTest extends FunSuite {

  private val deltas = Seq(DropPhases(1), BlockLanguageDelta) ++
    Delta.spliceAndFilterTop(
      JavaLanguage.blockWithVariables,
      JavaLanguage.javaClassSkeleton,
      Seq(SolveConstraintsDelta, ClearPhases))
  private val language = Delta.buildLanguage(deltas)

    test("int variable") {

      val program =
        """int x;
          |x = 3;
        """.stripMargin
      val getProgram = () => SourceUtils.stringToStream(program)
      val server = new LanguageServer(getProgram, language)
      val result = server.go(Position(2, 1))
      assertResult(SourceRange(Position(1,5), Position(1,6)))(result)
    }


  test("defined inside if") {
    val program =
      """int x;
        |if (true) {
        |  int y = 2;
        |  x += y;
        |}
      """.stripMargin
    val getProgram = () => SourceUtils.stringToStream(program)
    val server = new LanguageServer(getProgram, language)
    val xDefinition = server.go(Position(4, 3))
    assertResult(SourceRange(Position(1,5), Position(1,6)))(xDefinition)

    val yDefinition = server.go(Position(4, 8))
    assertResult(SourceRange(Position(3,7), Position(3,8)))(yDefinition)
  }
}
