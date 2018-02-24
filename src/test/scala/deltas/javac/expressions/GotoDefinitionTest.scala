package deltas.javac.expressions

import core.deltas.Delta
import core.language.LanguageServer
import core.language.node.SourceRange
import deltas.javac.JavaLanguage
import deltas.javac.methods.BlockLanguageDelta
import org.scalatest.FunSuite
import util.SourceUtils

class GotoDefinitionTest extends FunSuite {

  private val blockLanguage = Delta.buildLanguage(Seq(DropPhases(1), BlockLanguageDelta) ++ JavaLanguage.blockWithVariables)

  test("int variable") {
    val program =
      """int x;
        |x = 3;
      """.stripMargin
    val getProgram = () => SourceUtils.stringToStream(program)
    val server = new LanguageServer(getProgram, blockLanguage)
    val result = server.go(server.toPosition(2, 1))
    assertResult(SourceRange(server.toPosition(1,5), server.toPosition(1,6)))(result)
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
    val server = new LanguageServer(getProgram, blockLanguage)
    val xDefinition = server.go(server.toPosition(4, 3))
    assertResult(SourceRange(server.toPosition(1,5), server.toPosition(1,6)))(xDefinition)

    val yDefinition = server.go(server.toPosition(4, 8))
    assertResult(SourceRange(server.toPosition(3,7), server.toPosition(3,8)))(yDefinition)
  }

  test("fibonacci") {
    val program = SourceUtils.getJavaTestFileContents("Fibonacci")
    val getProgram = () => SourceUtils.stringToStream(program)
    val server = new LanguageServer(getProgram, JavaLanguage.getJava)
    val indexDefinition = server.go(server.toPosition(10, 16))
    assertResult(SourceRange(server.toPosition(8,37), server.toPosition(8,42)))(indexDefinition)

    val yDefinition = server.go(server.toPosition(5, 36))
    val methodRange = SourceRange(server.toPosition(8, 5), server.toPosition(11, 6))
    assertResult(methodRange)(yDefinition)
  }
}
