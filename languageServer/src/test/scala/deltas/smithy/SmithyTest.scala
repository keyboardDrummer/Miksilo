package deltas.smithy

import core.language.node.SourceRange
import langserver.types
import langserver.types.Location
import languageServer.{HumanPosition, LanguageServerTest, MiksiloLanguageServer}
import org.scalatest.FunSuite
import util.SourceUtils

class SmithyTest extends FunSuite with LanguageServerTest {

  test("example model") {
    val server = new MiksiloLanguageServer(SmithyLanguage.language)
    val program = SourceUtils.getTestFileContents("smithy/model.smithy")
    val diagnostics = getDiagnostic(server, program)
    assert(diagnostics.size == 1)
  }

  test("goto definition") {
    val server = new MiksiloLanguageServer(SmithyLanguage.language)
    val program = SourceUtils.getTestFileContents("smithy/model.smithy")
    val definition = gotoDefinition(server, program, HumanPosition(6,22)).head.range
    val expectation = types.Range(HumanPosition(98, 11), HumanPosition(98, 25))
    assertResult(expectation)(definition)
  }
}
