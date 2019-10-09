package languageServer

import core.language.FileRange
import org.eclipse.lsp4j.Location

object Conversions {

  implicit def convertPosition2(position: org.eclipse.lsp4j.Position): core.parsers.strings.Position = {
    core.parsers.strings.Position(position.getLine, position.getCharacter)
  }

  implicit def convertPosition(position: core.parsers.strings.Position): org.eclipse.lsp4j.Position = {
    new org.eclipse.lsp4j.Position(position.line, position.character)
  }

  implicit def convertRange2(range: org.eclipse.lsp4j.Range): core.parsers.strings.SourceRange = {
    core.parsers.strings.SourceRange(range.getStart, range.getEnd)
  }

  implicit def convertRange(range: core.parsers.strings.SourceRange): org.eclipse.lsp4j.Range = {
    new org.eclipse.lsp4j.Range(range.start, range.end)
  }

  implicit def convertFileRange(fileRange: FileRange): Location = {
    new Location(fileRange.uri, fileRange.range)
  }
}
