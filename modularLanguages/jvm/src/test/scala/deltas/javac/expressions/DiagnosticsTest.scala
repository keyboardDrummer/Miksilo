package deltas.javac.expressions

import core.parsers.editorParsers.SourceRange
import deltas.javac.JavaToByteCodeLanguage
import languageServer.{LanguageServerTest, MiksiloLanguageServer}
import lsp.{Diagnostic, HumanPosition}
import org.scalatest.funsuite.AnyFunSuite
import util.JavaSourceUtils

class DiagnosticsTest extends AnyFunSuite with LanguageServerTest {

  val server = new MiksiloLanguageServer(JavaToByteCodeLanguage.getJava)

  test("Reference cannot be resolved") {
    val program = JavaSourceUtils.getJavaTestFileContents("FibonacciBroken")
    val expectedResults = List(
      Diagnostic(SourceRange(HumanPosition(10,58), HumanPosition(10,64)), Some(1), "Could not find definition of index2", None, None),
      Diagnostic(SourceRange(HumanPosition(10,98), HumanPosition(10,104)), Some(1), "Could not find definition of index3", None, None))
    assertResult(expectedResults)(getDiagnostics(server, program))
  }
}

