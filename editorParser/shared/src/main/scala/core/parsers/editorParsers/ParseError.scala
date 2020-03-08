package core.parsers.editorParsers

import core.parsers.core.{TextPointer, ParseText}
import core.parsers.editorParsers.Position.PositionOrdering

case class TextEdit(range: SourceRange, newText: String)
case class Fix(title: String, edit: TextEdit)

/**
  * Position in a text document expressed as zero-based line and character offset.
  */
case class Position(line: Int, character: Int)

case class OffsetNodeRange(from: TextPointer, until: TextPointer) {
  def toSourceRange() = SourceRange(from.lineCharacter, until.lineCharacter)
  def toOffsetRange = OffsetRange(from.offset, until.offset)
}

case class OffsetRange(from: Int, until: Int) {
  def contains(offset: Int): Boolean = {
    from <= offset && offset <= until
  }
  def toRange(text: ParseText) = SourceRange(text.getPosition(from), text.getPosition(until))
}
case class FileOffsetRange(uri: String, range: OffsetRange)

/**
  * A range in a text document.
  */
case class SourceRange(start: Position, end: Position) {

  def contains(position: Position): Boolean = {
    PositionOrdering.lteq(start, position) && PositionOrdering.lteq(position, end)
  }

  def contains(position: SourceRange): Boolean = {
    PositionOrdering.lteq(start, position.start) && PositionOrdering.lteq(position.end, end)
  }
}

object Position {
  implicit object PositionOrdering extends Ordering[Position] {

    private val ordering = Ordering.by[Position, (Int, Int)](x => (x.line, x.character))
    override def compare(x: Position, y: Position): Int = {
      ordering.compare(x, y)
    }
  }
}

trait ParseError {
  def fix: Option[Fix] = None
  def message: String
  def from: TextPointer
  def to: TextPointer
  def range = OffsetNodeRange(from, to).toSourceRange()

  def canMerge: Boolean = false
  def penalty: Double
  def score: Double = -penalty * 1
  def append(other: ParseError): Option[ParseError] = None

  override def toString = s"$message AT $from"
}
