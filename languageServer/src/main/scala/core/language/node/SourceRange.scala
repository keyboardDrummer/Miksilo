package core.language.node

import core.language.node.Node.PositionOrdering
import langserver.types.Position

object SourceRange {
  implicit def toRange(sourceRange: SourceRange): langserver.types.Range = {
    langserver.types.Range(sourceRange.start, sourceRange.end)
  }
}

case class SourceRange(start: Position, end: Position) {

  def contains(position: Position): Boolean = {
    PositionOrdering.lteq(start, position) && PositionOrdering.lteq(position, end)
  }

  def contains(position: SourceRange): Boolean = {
    PositionOrdering.lteq(start, position.start) && PositionOrdering.lteq(position.end, end)
  }
}
