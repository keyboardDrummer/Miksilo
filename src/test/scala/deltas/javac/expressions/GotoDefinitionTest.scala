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

    test("block goto def") {
      val deltas = Seq(DropPhases(1), BlockLanguageDelta) ++
          Delta.spliceAndFilterTop(
                  JavaLanguage.blockWithVariables,
                  JavaLanguage.javaClassSkeleton,
                  Seq(SolveConstraintsDelta, ClearPhases))

      val program =
        """int x;
          |x = 3;
        """.stripMargin
      val getProgram = () => SourceUtils.stringToStream(program)
      val language = Delta.buildLanguage(deltas)
      val server = new LanguageServer(getProgram, language)
      val result = server.go(Position(2, 1))
      assertResult(SourceRange(Position(1,5), Position(1,6)))(result)
    }
}
