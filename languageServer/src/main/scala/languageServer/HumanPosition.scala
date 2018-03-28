package languageServer

import langserver.types.Position

object HumanPosition {
  implicit def toPosition(position: HumanPosition): Position = new Position(position.line - 1, position.character - 1)
  implicit def fromPosition(position: Position): HumanPosition = new HumanPosition(position.line + 1, position.character + 1)
}

case class HumanPosition(line: Int, character: Int) {}
