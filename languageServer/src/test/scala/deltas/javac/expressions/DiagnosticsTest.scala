package deltas.javac.expressions

import core.language.node.SourceRange
import deltas.javac.JavaToByteCodeLanguage
import langserver.types.Diagnostic
import languageServer.{HumanPosition, LanguageServerTest, MiksiloLanguageServer}
import org.scalatest.FunSuite
import util.SourceUtils

class DiagnosticsTest extends FunSuite with LanguageServerTest {

  val server = new MiksiloLanguageServer(JavaToByteCodeLanguage.getJava)

  test("Reference cannot be resolved") {
    val program = SourceUtils.getJavaTestFileContents("FibonacciBroken")
    val expectedResults = List(
      Diagnostic(SourceRange(HumanPosition(10,58), HumanPosition(10,64)),Some(1),None,None, "Could not find definition of index2"),
      Diagnostic(SourceRange(HumanPosition(10,98), HumanPosition(10,104)),Some(1),None,None, "Could not find definition of index3"))
    assertResult(expectedResults)(getDiagnostics(server, program))
  }
}

