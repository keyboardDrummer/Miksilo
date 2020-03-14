package deltas.javac.expressions

import miksilo.editorParser.parsers.editorParsers.SourceRange
import deltas.javac.JavaToByteCodeLanguage
import languageServer.{LanguageServerTest, MiksiloLanguageServer}
import miksilo.lspprotocol.lsp.{Diagnostic, DidChangeTextDocumentParams, HumanPosition, VersionedTextDocumentIdentifier}
import org.scalatest.funsuite.AnyFunSuite
import util.JavaSourceUtils

class DiagnosticsTest extends AnyFunSuite with LanguageServerTest {

  val server = new MiksiloLanguageServer(JavaToByteCodeLanguage.getJava)

  test("Reference cannot be resolved") {
    val program = JavaSourceUtils.getJavaTestFileContents("FibonacciBroken")
    val expectedResults = List(
      Diagnostic(SourceRange(HumanPosition(10,58), HumanPosition(10,64)), Some(1), "Could not find definition of index2", None, None),
      Diagnostic(SourceRange(HumanPosition(10,98), HumanPosition(10,104)), Some(1), "Could not find definition of index3", None, None))
    val (diagnostics, document) = openAndCheckDocument(server, program)
    assertResult(expectedResults)(diagnostics)

    val diagnostics2 = getDiagnostics(server, DidChangeTextDocumentParams(VersionedTextDocumentIdentifier(document.uri, 0L), Seq.empty))
    assertResult(diagnostics2)(diagnostics)
  }
}

