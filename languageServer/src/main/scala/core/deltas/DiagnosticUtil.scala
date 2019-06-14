package core.deltas

import core.language.node.SourceRange
import langserver.types.{Diagnostic, DiagnosticSeverity, Position}
import languageServer.HumanPosition
import core.bigrammar.BiGrammarToParser._

object DiagnosticUtil {

  def getDiagnosticFromParseFailure(errors: List[MyParseError]): List[Diagnostic] = errors.map(error => {
    val range = SourceRange(error.from.position, error.to.position)
    Diagnostic(range, Some(DiagnosticSeverity.Error), None, None, error.message)
  })
}
