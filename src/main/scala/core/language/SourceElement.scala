package core.language

import scala.util.parsing.input.Position

trait SourceElement {
  def start: Position
  def end: Position

  def contains(position: Position): Boolean = ???
}
