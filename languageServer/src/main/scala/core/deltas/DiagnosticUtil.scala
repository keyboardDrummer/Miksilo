package core.deltas

import core.bigrammar.BiGrammarToParser._
import core.language.node.SourceRange
import core.smarts.FileDiagnostic
import languageServer.{CodeAction, Diagnostic, DiagnosticSeverity, TextEdit, WorkspaceEdit}

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
    val codeActionEdit: Option[TextEdit] = error match {
      case missingInput: MissingInput if missingInput.insertFix.trim != "" =>
        Some(TextEdit(SourceRange(missingInput.from.position, missingInput.from.position), missingInput.insertFix.trim))
      case dropError: DropError =>
        Some(TextEdit(SourceRange(dropError.from.position, dropError.to.position), ""))
      case _ => None
    }
    val codeAction = codeActionEdit.map(edit =>
      CodeAction("Fix syntax", "quickfix", Some(Seq(diagnostic.identifier)), Some(WorkspaceEdit(Map(uri -> Seq(edit)))))
    )
    (FileDiagnostic(uri, diagnostic), codeAction)
  }
}
