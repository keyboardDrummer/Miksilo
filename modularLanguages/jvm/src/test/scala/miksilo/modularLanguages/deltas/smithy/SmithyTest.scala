package miksilo.modularLanguages.deltas.smithy

import miksilo.editorParser.parsers.editorParsers.SourceRange
import miksilo.editorParser.SourceUtils
import miksilo.languageServer.server.{LanguageServerTest, MiksiloLanguageServer}
import miksilo.lspprotocol.lsp.HumanPosition
import org.scalatest.funsuite.AnyFunSuite
class SmithyTest extends AnyFunSuite with LanguageServerTest {

  val server = new MiksiloLanguageServer(SmithyLanguage.language)
  val program = SourceUtils.getResourceFileContents("smithy/model.smithy")

  test("example model") {
    val diagnostics = getDiagnostics(server, program)
    assert(diagnostics.size == 1)
  }

  test("goto definition") {
    val definition = gotoDefinition(server, program, HumanPosition(6,22)).head.range
    val expectation = SourceRange(HumanPosition(98, 11), HumanPosition(98, 25))
    assertResult(expectation)(definition)
  }

  test("goto definition $") {
    val definition = gotoDefinition(server, program, HumanPosition(76, 35)).head.range
    val expectation = SourceRange(HumanPosition(71, 3), HumanPosition(71, 12))
    assertResult(expectation)(definition)
  }
}
