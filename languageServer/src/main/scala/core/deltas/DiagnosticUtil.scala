package core.deltas

import core.bigrammar.BiGrammarToParser._
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
    val codeActionEdit: Option[(String, TextEdit)] = error match {
      case missingInput: MissingInput if missingInput.insertFix.trim != "" =>
        Some("Insert missing symbols" -> TextEdit(SourceRange(missingInput.from.position, missingInput.from.position), missingInput.insertFix.trim))
      case dropError: DropError =>
        Some("Remove unexpected symbols" -> TextEdit(SourceRange(dropError.from.position, dropError.to.position), ""))
      case _ => None
    }
    val codeAction = codeActionEdit.map(edit =>
      CodeAction(edit._1, "quickfix", Some(Seq(diagnostic.identifier)), Some(WorkspaceEdit(Map(uri -> Seq(edit._2)))))
    )
    (FileDiagnostic(uri, diagnostic), codeAction)
  }
}
