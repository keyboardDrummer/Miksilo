package deltas.smithy

import languageServer.{LanguageServerTest, MiksiloLanguageServer}
import org.scalatest.FunSuite
import util.SourceUtils

class SmithyTest extends FunSuite with LanguageServerTest {

  test("example model") {
    val server = new MiksiloLanguageServer(SmithyLanguage.language)
    val program = SourceUtils.getTestFileContents("smithy/model.smithy")
    val diagnostics = getDiagnostic(server, program)
    assert(diagnostics.isEmpty)
  }
}
