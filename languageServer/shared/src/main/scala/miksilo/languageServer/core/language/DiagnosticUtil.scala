package miksilo.languageServer.core.language

import miksilo.editorParser.parsers.editorParsers.ParseError
import miksilo.languageServer.core.smarts.FileDiagnostic
import miksilo.lspprotocol.lsp._

object DiagnosticUtil {

  def getDiagnosticsFromParseFailures(file: String, errors: Seq[ParseError]): Map[FileDiagnostic, Seq[CodeAction]] = {
    var result = Map.empty[FileDiagnostic, Seq[CodeAction]]

    for((diagnostic, codeActionOption) <- errors.map(error => getDiagnosticsFromParseFailure(file, error))) {
      result += diagnostic -> codeActionOption.toSeq
    }

    result
  }

  def getDiagnosticsFromParseFailure(uri: String, error: ParseError): (FileDiagnostic, Option[CodeAction]) = {
    val diagnostic = Diagnostic(error.range, Some(DiagnosticSeverity.Error), error.message, None, None)
    val codeAction = error.fix.map(fix =>
      CodeAction(fix.title, "quickfix", Some(Seq(diagnostic.identifier)), Some(WorkspaceEdit(Map(uri -> Seq(fix.edit)))))
    )
    (FileDiagnostic(uri, diagnostic), codeAction)
  }
}
