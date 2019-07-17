package core.language.node

import core.language.node.Node.PositionOrdering
import languageServer.Position
import languageServer.Range

object SourceRange {
  implicit def toRange(sourceRange: SourceRange): Range = {
    Range(sourceRange.start, sourceRange.end)
  }
}

case class FileRange(uri: String, range: SourceRange) {
  def contains(filePosition: FilePosition): Boolean = {
    uri == filePosition.uri && range.contains(filePosition.position)
  }
}

case class FilePosition(uri: String, position: Position)

case class SourceRange(start: Position, end: Position) {

  def contains(position: Position): Boolean = {
    PositionOrdering.lteq(start, position) && PositionOrdering.lteq(position, end)
  }

  def contains(position: SourceRange): Boolean = {
    PositionOrdering.lteq(start, position.start) && PositionOrdering.lteq(position.end, end)
  }
}
