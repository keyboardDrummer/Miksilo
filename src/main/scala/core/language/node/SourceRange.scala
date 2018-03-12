package core.language.node

import core.language.node.Node.PositionOrdering
import langserver.types.Position

case class SourceRange(start: Position, end: Position) {

  def contains(position: Position): Boolean = {
    PositionOrdering.lteq(start, position) && PositionOrdering.lteq(position, end)
  }
}
