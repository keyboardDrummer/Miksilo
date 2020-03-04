package languageServer

import core.language.Language
import core.parsers.editorParsers.{Position, SourceRange}
import lsp._

import scala.util.Random

trait LanguageServerTest {

  def documentSymbols(server: LanguageServer, program: String): Seq[SymbolInformation] = {
    val document = openDocument(server, program)
    server.asInstanceOf[DocumentSymbolProvider].documentSymbols(DocumentSymbolParams(document))
  }

  def gotoDefinition(language: Language, program: String, position: HumanPosition): Seq[FileRange] = {
    val server = new MiksiloLanguageServer(language)
    gotoDefinition(server, program, position)
  }

  def rename(server: LanguageServer, program: String, position: Position, newName: String): WorkspaceEdit = {
    val document = openDocument(server, program)
    server.asInstanceOf[RenameProvider].rename(RenameParams(document, position, newName))
  }

  def gotoDefinition(server: LanguageServer, program: String, position: HumanPosition): Seq[FileRange] = {
    val document = openDocument(server, program)
    server.asInstanceOf[DefinitionProvider].gotoDefinition(DocumentPosition(document, position))
  }

  def references(server: LanguageServer, program: String, position: HumanPosition, includeDeclaration: Boolean): collection.Seq[FileRange] = {
    val document = openDocument(server, program)
    server.asInstanceOf[ReferencesProvider].references(ReferencesParams(document, position, ReferenceContext(includeDeclaration)))
  }

  def createCompletionItem(value: String) = CompletionItem(value, Some(CompletionItemKind.Variable), insertText = Some(value))

  def complete(server: LanguageServer, program: String, position: HumanPosition): CompletionList = {
    val document = openDocument(server, program)
    server.asInstanceOf[CompletionProvider].complete(DocumentPosition(document, position))
  }

  def applyChange(server: MiksiloLanguageServer, document: TextDocumentIdentifier, from: Int, until: Int, newText: String): Seq[Diagnostic] = {
    getDiagnostics(server, DidChangeTextDocumentParams(new VersionedTextDocumentIdentifier(document.uri, 0L),
      Seq(TextDocumentContentChangeEvent(Some(SourceRange(Position(0, from), Position(0, until))), Some(until - from), newText))))
  }

  def getDiagnostics(server: LanguageServer, change: DidChangeTextDocumentParams): Seq[Diagnostic] = {
    var result: Seq[Diagnostic] = null
    server.setClient(new LanguageClient {
      override def sendDiagnostics(diagnostics: PublishDiagnostics): Unit = {
        result = diagnostics.diagnostics
      }

      override def trackMetric(name: String, value: Double): Unit = {}
    })
    server.didChange(change)
    result
  }

  def getDiagnostics(server: LanguageServer, program: String): Seq[Diagnostic] = {
    openAndCheckDocument(server, program)._1
  }

  val random = new Random()
  def openAndCheckDocument(server: LanguageServer, content: String, uri: String = Random.nextInt().toString): (Seq[Diagnostic],TextDocumentIdentifier) = {

    var result: Seq[Diagnostic] = null
    server.setClient(new LanguageClient {
      override def sendDiagnostics(diagnostics: PublishDiagnostics): Unit = {
        result = diagnostics.diagnostics
      }

      override def trackMetric(name: String, value: Double): Unit = {}
    })

    val item = new TextDocumentItem(uri, "", 1, content)
    server.didOpen(item)
    (result, TextDocumentIdentifier(item.uri))
  }

  def openDocument(server: LanguageServer, content: String, uri: String = Random.nextInt().toString): TextDocumentIdentifier = {
    openAndCheckDocument(server, content, uri)._2
  }
}
