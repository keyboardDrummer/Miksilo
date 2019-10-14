package core.language

import core.parsers.editorParsers.{ParseError}
import core.parsers.strings.StringReaderLike
import core.smarts.FileDiagnostic
import languageServer._

object DiagnosticUtil {

  def getDiagnosticsFromParseFailures[Input <: StringReaderLike[Input]](file: String, errors: Seq[ParseError[Input]]): Map[FileDiagnostic, Seq[CodeAction]] = {
    var result = Map.empty[FileDiagnostic, Seq[CodeAction]]

    for((diagnostic, codeActionOption) <- errors.map(error => getDiagnosticsFromParseFailure(file, error))) {
      result += diagnostic -> codeActionOption.toSeq
    }

    result
  }

  def getDiagnosticsFromParseFailure[Input <: StringReaderLike[Input]](uri: String, error: ParseError[Input]): (FileDiagnostic, Option[CodeAction]) = {
    val range = SourceRange(error.from.position, error.to.position)
    val diagnostic = Diagnostic(range, Some(DiagnosticSeverity.Error), error.message, None, None)
    val codeAction = error.fix.map(fix =>
      CodeAction(fix.title, "quickfix", Some(Seq(diagnostic.identifier)), Some(WorkspaceEdit(Map(uri -> Seq(fix.edit)))))
    )
    (FileDiagnostic(uri, diagnostic), codeAction)
  }
}
