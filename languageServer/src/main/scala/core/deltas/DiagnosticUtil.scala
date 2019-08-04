package core.deltas

import core.bigrammar.BiGrammarToParser._
import core.parsers.editorParsers.Fix
import core.smarts.FileDiagnostic
import languageServer.{CodeAction, Diagnostic, DiagnosticSeverity, SourceRange, TextEdit, WorkspaceEdit}

object DiagnosticUtil {

  def getDiagnosticsFromParseFailures(file: String, errors: Seq[MyParseError]): Map[FileDiagnostic, Seq[CodeAction]] = {
    var result = Map.empty[FileDiagnostic, Seq[CodeAction]]

    for((diagnostic, codeActionOption) <- errors.map(error => getDiagnosticsFromParseFailure(file, error))) {
      result += diagnostic -> codeActionOption.toSeq
    }

    result
  }

  def getDiagnosticsFromParseFailure(uri: String, error: MyParseError): (FileDiagnostic, Option[CodeAction]) = {
    val range = SourceRange(error.from.position, error.to.position)
    val diagnostic = Diagnostic(range, Some(DiagnosticSeverity.Error), None, None, error.message)
    val codeAction = error.fix.map(fix =>
      CodeAction(fix.title, "quickfix", Some(Seq(diagnostic.identifier)), Some(WorkspaceEdit(Map(uri -> Seq(fix.edit)))))
    )
    (FileDiagnostic(uri, diagnostic), codeAction)
  }
}
