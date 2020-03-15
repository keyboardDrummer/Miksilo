package languageServer

import miksilo.editorParser.parsers.editorParsers.SourceRange
import miksilo.languageServer.core.TestUtils
import languages.{JsonLanguage, YamlLanguage}
import miksilo.lspprotocol.lsp._
import miksilo.editorParser.SourceUtils
import org.scalatest.funsuite.AnyFunSuite

class IncrementalParsingPerformanceTest extends AnyFunSuite with LanguageServerTest {

  test("Make small edit performance JSON") {

    val server = new MiksiloLanguageServer(JsonLanguage)
    val source = SourceUtils.getResourceFileContents("AutoScalingMultiAZWithNotifications.json")

    val document = new TextDocumentItem("item","language", 0L, source)
    val documentId = VersionedTextDocumentIdentifier(document.uri, document.version)
    server.didOpen(document)
    val position = HumanPosition(445, 35)
    val positionPlusOne = HumanPosition(445, 36)
    var diagnostics: PublishDiagnostics = null
    val client = new LanguageClient {
      override def sendDiagnostics(newDiagnostics: PublishDiagnostics): Unit = diagnostics = newDiagnostics

      override def trackMetric(name: String, value: Double): Unit =  {
      }
    }
    server.setClient(client)

    TestUtils.runPerformanceTest(60, 100, () => {
      server.didChange(DidChangeTextDocumentParams(documentId,
        Seq(TextDocumentContentChangeEvent(Some(SourceRange(position, position)), None, "2"))))
      assert(diagnostics.diagnostics.isEmpty)
      server.didChange(DidChangeTextDocumentParams(documentId,
        Seq(TextDocumentContentChangeEvent(Some(SourceRange(position, positionPlusOne)), None, ""))))
      assert(diagnostics.diagnostics.isEmpty)
    })
  }

  test("Make small edit performance YAML") {

    val server = new MiksiloLanguageServer(YamlLanguage)

    val source = SourceUtils.getResourceFileContents("AutoScalingMultiAZWithNotifications.yaml")

    val document = new TextDocumentItem("item","language", 0L, source)
    val documentId = VersionedTextDocumentIdentifier(document.uri, document.version)
    server.didOpen(document)
    val position = HumanPosition(423, 36)
    val positionPlusOne = HumanPosition(423, 37)
    var diagnostics: PublishDiagnostics = null
    val client = new LanguageClient {
      override def sendDiagnostics(newDiagnostics: PublishDiagnostics): Unit = diagnostics = newDiagnostics

      override def trackMetric(name: String, value: Double): Unit =  {
      }
    }
    server.setClient(client)

    TestUtils.runPerformanceTest(130, 100, () => {
      server.didChange(DidChangeTextDocumentParams(documentId,
        Seq(TextDocumentContentChangeEvent(Some(SourceRange(position, position)), None, "A"))))
      assert(diagnostics.diagnostics.isEmpty)
      server.didChange(DidChangeTextDocumentParams(documentId,
        Seq(TextDocumentContentChangeEvent(Some(SourceRange(position, positionPlusOne)), None, ""))))
      assert(diagnostics.diagnostics.isEmpty)
    })
  }

}
