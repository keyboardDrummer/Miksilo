package core.language

import core.parsers.core.ParseInput
import core.parsers.editorParsers.{ParseError, SourceRange}
import core.smarts.FileDiagnostic
import lsp._

object DiagnosticUtil {

  def getDiagnosticsFromParseFailures[Input <: ParseInput](file: String, errors: Seq[ParseError[Input]]): Map[FileDiagnostic, Seq[CodeAction]] = {
    var result = Map.empty[FileDiagnostic, Seq[CodeAction]]

    for((diagnostic, codeActionOption) <- errors.map(error => getDiagnosticsFromParseFailure(file, error))) {
      result += diagnostic -> codeActionOption.toSeq
    }

    result
  }

  def getDiagnosticsFromParseFailure[Input <: ParseInput](uri: String, error: ParseError[Input]): (FileDiagnostic, Option[CodeAction]) = {
    val diagnostic = Diagnostic(error.range, Some(DiagnosticSeverity.Error), error.message, None, None)
    val codeAction = error.fix.map(fix =>
      CodeAction(fix.title, "quickfix", Some(Seq(diagnostic.identifier)), Some(WorkspaceEdit(Map(uri -> Seq(fix.edit)))))
    )
    (FileDiagnostic(uri, diagnostic), codeAction)
  }
}
