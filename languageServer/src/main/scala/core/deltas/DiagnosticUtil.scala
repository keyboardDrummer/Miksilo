package core.deltas

import core.language.node.SourceRange
import langserver.types.{Diagnostic, DiagnosticSeverity, Position}
import languageServer.HumanPosition
import core.bigrammar.BiGrammarToParser._

object DiagnosticUtil {

  def getDiagnosticFromParseFailure(failure: ParseFailure[Any]): Diagnostic = {
    val position = failure.remainder.position
    Diagnostic(SourceRange(position, Position(position.line, position.character + 1)), Some(DiagnosticSeverity.Error), None, None, failure.message)
  }

  private val rowColumnRegex = """\[(\d*)\.(\d*)\] failure: ((.|\n)*)\n\n""".r
  def getDiagnosticFromParseException(message: String): Diagnostic = {
    try {
      val messageMatch = rowColumnRegex.findFirstMatchIn(message).get
      val row = messageMatch.group(1).toInt
      val column = messageMatch.group(2).toInt
      Diagnostic(SourceRange(HumanPosition(row, column), HumanPosition(row, column + 1)), Some(DiagnosticSeverity.Error), None, None, messageMatch.group(3))

    } catch
      {
        case e: java.util.NoSuchElementException => throw new Exception("Failed to parse message " + message)
      }
  }
}
