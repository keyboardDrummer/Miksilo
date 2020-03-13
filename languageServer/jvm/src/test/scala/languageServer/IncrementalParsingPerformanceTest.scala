package languageServer

import core.parsers.editorParsers.SourceRange
import core.{SourceUtils, TestUtils}
import languages.{JsonLanguage, YamlLanguage}
import lsp._
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
        System.out.println("name: " + name +", value: " + value)
      }
    }
    server.setClient(client)

    TestUtils.runPerformanceTest(30, 100, () => {
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
        System.out.println("name: " + name +", value: " + value)
      }
    }
    server.setClient(client)

    TestUtils.runPerformanceTest(100, 100, () => {
      server.didChange(DidChangeTextDocumentParams(documentId,
        Seq(TextDocumentContentChangeEvent(Some(SourceRange(position, position)), None, "A"))))
      assert(diagnostics.diagnostics.isEmpty)
      server.didChange(DidChangeTextDocumentParams(documentId,
        Seq(TextDocumentContentChangeEvent(Some(SourceRange(position, positionPlusOne)), None, ""))))
      assert(diagnostics.diagnostics.isEmpty)
    })
  }

}
