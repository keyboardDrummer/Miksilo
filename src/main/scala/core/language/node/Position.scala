package core.language.node

class Position(line: Int, character: Int) extends langserver.types.Position(line, character) with Ordered[Position] { //TODO remove in favor of other thing.
  override def compare(that: Position): Int = {
    line.compare(that.line) match {
      case 0 => character.compare(that.character)
      case x => x
    }
  }
}
