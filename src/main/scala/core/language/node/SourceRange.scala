package core.language.node

case class SourceRange(start: Position, end: Position) {
  def contains(position: Position): Boolean = {
    start <= position && position <= end
  }
}
