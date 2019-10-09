package languageServer

import java.util.concurrent.CompletableFuture

import languageServer.Conversions._
import org.eclipse.lsp4j
import org.eclipse.lsp4j.{InitializeResult, InitializedParams, MessageParams, ServerCapabilities, ShowMessageRequestParams, services}

import scala.collection.JavaConverters

class LanguageClientToLSP4Client(languageClient: LanguageClient) extends
  org.eclipse.lsp4j.services.LanguageClient {

  override def telemetryEvent(`object`: Any) = ???

  override def publishDiagnostics(diagnostics: lsp4j.PublishDiagnosticsParams) = ???

  override def showMessage(messageParams: MessageParams) = ???

  override def showMessageRequest(requestParams: ShowMessageRequestParams) = ???

  override def logMessage(message: MessageParams) = ???
}

class LanguageServerToLSP4JServer(languageServer: LanguageServer)
  extends org.eclipse.lsp4j.services.LanguageServer {

  def setClient(languageClient: services.LanguageClient): Unit = {
    languageServer.setClient(new LanguageClient {
      override def sendDiagnostics(diagnostics: PublishDiagnosticsParams): Unit = {
        languageClient.publishDiagnostics(new lsp4j.PublishDiagnosticsParams(diagnostics.uri,
          JavaConverters.seqAsJavaList(diagnostics.diagnostics.map(d =>
            new lsp4j.Diagnostic(d.range, d.message, d.severity.get, null)))))
      }
    })
  }

  override def initialized(params: InitializedParams): Unit = {
  }

  override def initialize(params: lsp4j.InitializeParams): CompletableFuture[InitializeResult] = {
    val capabilities = new ServerCapabilities()
    languageServer match {
      case completionProvider: CompletionProvider =>
        val options = new lsp4j.CompletionOptions(completionProvider.getOptions.resolveProvider,
          JavaConverters.seqAsJavaList(completionProvider.getOptions.triggerCharacters))
        capabilities.setCompletionProvider(options)
    }
    languageServer match {
      case _: DefinitionProvider =>
        capabilities.setDefinitionProvider(true)
    }
    languageServer match {
      case _: CodeActionProvider =>
        capabilities.setCodeActionProvider(true)
    }
    languageServer match {
      case _: DocumentSymbolProvider =>
        capabilities.setDocumentSymbolProvider(true)
    }
    languageServer match {
      case _: ReferencesProvider =>
        capabilities.setReferencesProvider(true)
    }
    languageServer match {
      case _: RenameProvider =>
        capabilities.setRenameProvider(true)
    }

    CompletableFuture.completedFuture(new lsp4j.InitializeResult(capabilities))
  }

  override def shutdown() = {
    CompletableFuture.completedFuture(null)
  }

  override def exit(): Unit = {

  }

  val textDocumentService = new TextDocumentServiceFromLanguageServer(languageServer)
  override def getTextDocumentService: TextDocumentServiceFromLanguageServer = textDocumentService

  val workspaceService = new LanguageServerToWorkpaceService(languageServer)
  override def getWorkspaceService = workspaceService
}


