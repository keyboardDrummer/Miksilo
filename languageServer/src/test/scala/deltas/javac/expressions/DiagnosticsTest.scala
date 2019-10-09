package deltas.javac.expressions

import core.parsers.strings.SourceRange
import deltas.javac.JavaToByteCodeLanguage
import languageServer.{Diagnostic, HumanPosition, LanguageServerTest, MiksiloLanguageServer}
import org.eclipse.lsp4j.DiagnosticSeverity
import org.scalatest.FunSuite
import util.SourceUtils

class DiagnosticsTest extends FunSuite with LanguageServerTest {

  val server = new MiksiloLanguageServer(JavaToByteCodeLanguage.getJava)

  test("Reference cannot be resolved") {
    val program = SourceUtils.getJavaTestFileContents("FibonacciBroken")
    val expectedResults = List(
      Diagnostic(SourceRange(HumanPosition(10,58), HumanPosition(10,64)),"Could not find definition of index2",Some(DiagnosticSeverity.Error)),
      Diagnostic(SourceRange(HumanPosition(10,98), HumanPosition(10,104)),"Could not find definition of index3", Some(DiagnosticSeverity.Error)))
    assertResult(expectedResults)(getDiagnostic(server, program))
  }
}

