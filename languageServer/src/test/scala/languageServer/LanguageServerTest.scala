package languageServer

import core.language.Language
import core.language.node.FileRange
import langserver.types._
import languageServer.lsp._
import org.scalatest.FunSuite

import scala.util.Random

trait LanguageServerTest extends FunSuite {

  val itemUri = "helloWorld"
  def gotoDefinition(language: Language, program: String, position: HumanPosition): Seq[FileRange] = {
    val server = new MiksiloLanguageServer(language)
    gotoDefinition(server, program, position)
  }

  def gotoDefinition(server: LanguageServer, program: String, position: HumanPosition): Seq[FileRange] = {
    val document = openDocument(server, program)
    server.asInstanceOf[DefinitionProvider].gotoDefinition(DocumentPosition(document, position))
  }

  def references(server: LanguageServer, program: String, position: HumanPosition, includeDeclaration: Boolean): Seq[FileRange] = {
    val document = openDocument(server, program)
    server.asInstanceOf[ReferencesProvider].references(ReferencesParams(document, position, ReferenceContext(includeDeclaration)))
  }

  def complete(server: LanguageServer, program: String, position: HumanPosition): CompletionList = {
    val document = openDocument(server, program)
    server.asInstanceOf[CompletionProvider].complete(DocumentPosition(document, position))
  }

  def getDiagnostics(server: LanguageServer, program: String): Seq[Diagnostic] = {
    var result: Seq[Diagnostic] = null
    val document = openDocument(server, program)
    server.setClient(new LanguageClient {
      override def sendDiagnostics(diagnostics: PublishDiagnostics): Unit = {
        result = diagnostics.diagnostics
      }
    })
    server.didChange(DidChangeTextDocumentParams(VersionedTextDocumentIdentifier(document.uri, 0), Seq.empty))
    result
  }

  val random = new Random()
  def openDocument(server: LanguageServer, content: String, uri: String = itemUri): TextDocumentIdentifier = {
    val item = new TextDocumentItem(uri, "", 1, content)
    server.didOpen(item)
    TextDocumentIdentifier(item.uri)
  }
}
