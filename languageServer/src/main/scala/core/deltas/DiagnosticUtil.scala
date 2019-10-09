package core.deltas

import core.bigrammar.BiGrammarToParser._
import core.language.FileRange
import core.parsers.strings.SourceRange
import core.smarts.FileDiagnostic
import languageServer.{CodeAction, Diagnostic, TextEdit, WorkspaceEdit}
import org.eclipse.lsp4j.{DiagnosticSeverity, Location}

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
    val diagnostic = Diagnostic(range, error.message, Some(DiagnosticSeverity.Error))
    val codeAction = error.fix.map(fix => {
      val lspEdit = TextEdit(fix.edit.range, fix.edit.newText)
      val edit = new WorkspaceEdit(Map(uri -> Seq(lspEdit)))
      CodeAction(fix.title, "quickfix", Some(Seq(diagnostic.identifier)), Some(edit))
    })
    (FileDiagnostic(uri, diagnostic), codeAction)
  }
}
