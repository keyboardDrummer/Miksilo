package core.deltas

import core.language.node.SourceRange
import core.parsers.editorParsers.ParseFailure
import core.parsers.strings.{StringParserWriter, StringReader}
import langserver.types.{Diagnostic, DiagnosticSeverity}
import languageServer.HumanPosition

object DiagnosticUtil extends StringParserWriter {

  def getDiagnosticFromParseFailure(failure: ParseFailure[StringReader, Any]): Diagnostic = {
    val row = failure.remainder.position.line
    val column = failure.remainder.position.column
    Diagnostic(SourceRange(HumanPosition(row, column), HumanPosition(row, column + 1)), Some(DiagnosticSeverity.Error), None, None, failure.message)
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
