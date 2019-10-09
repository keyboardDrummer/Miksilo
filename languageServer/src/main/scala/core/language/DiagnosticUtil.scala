package core.language

import core.parsers.editorParsers.ExternalParseError
import core.parsers.strings.{StringReaderLike}
import core.smarts.FileDiagnostic
import languageServer._

object DiagnosticUtil {

  def getDiagnosticsFromParseFailures[Input](file: String, errors: Seq[ExternalParseError[StringReaderLike[Input]]]): Map[FileDiagnostic, Seq[CodeAction]] = {
    var result = Map.empty[FileDiagnostic, Seq[CodeAction]]

    for((diagnostic, codeActionOption) <- errors.map(error => getDiagnosticsFromParseFailure(file, error))) {
      result += diagnostic -> codeActionOption.toSeq
    }

    result
  }

  def getDiagnosticsFromParseFailure[Input](uri: String, error: ExternalParseError[StringReaderLike[Input]]): (FileDiagnostic, Option[CodeAction]) = {
    val range = SourceRange(error.from.position, error.to.position)
    val diagnostic = Diagnostic(range, Some(DiagnosticSeverity.Error), None, None, error.message)
    val codeAction = error.fix.map(fix =>
      CodeAction(fix.title, "quickfix", Some(Seq(diagnostic.identifier)), Some(WorkspaceEdit(Map(uri -> Seq(fix.edit)))))
    )
    (FileDiagnostic(uri, diagnostic), codeAction)
  }
}
