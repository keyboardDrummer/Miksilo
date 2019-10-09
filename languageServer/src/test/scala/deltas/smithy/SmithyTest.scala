package deltas.smithy

import core.parsers.strings.SourceRange
import languageServer.{HumanPosition, LanguageServerTest, MiksiloLanguageServer}
import org.scalatest.FunSuite
import util.SourceUtils

class SmithyTest extends FunSuite with LanguageServerTest {

  val server = new MiksiloLanguageServer(SmithyLanguage.language)
  val program = SourceUtils.getTestFileContents("smithy/model.smithy")

  test("example model") {
    val diagnostics = getDiagnostic(server, program)
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
