package languageServer

import org.eclipse.lsp4j.services.WorkspaceService
import org.eclipse.lsp4j.{DidChangeConfigurationParams, DidChangeWatchedFilesParams}

class LanguageServerToWorkpaceService(languageServer: LanguageServer) extends WorkspaceService {
  override def didChangeConfiguration(params: DidChangeConfigurationParams) = ???

  override def didChangeWatchedFiles(params: DidChangeWatchedFilesParams) = ???
}
