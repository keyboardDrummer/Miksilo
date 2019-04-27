package core.deltas

import core.language.node.SourceRange
import langserver.types.{Diagnostic, DiagnosticSeverity, Position}
import languageServer.HumanPosition
import core.bigrammar.BiGrammarToParser._

object DiagnosticUtil {

  def getDiagnosticFromParseFailure(errors: List[ParseError]): List[Diagnostic] = {
    errors.map(error => {
      val position = error.location.position
      Diagnostic(SourceRange(position, Position(position.line, position.character + 1)), Some(DiagnosticSeverity.Error), None, None, error.message)
    })
  }
}
