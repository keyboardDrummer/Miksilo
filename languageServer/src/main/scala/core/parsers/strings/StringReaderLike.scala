package core.parsers.strings

import core.parsers.core.ParseInput
import core.parsers.sequences.SequenceInput
import languageServer.Position

trait StringReaderLike[Input] extends SequenceInput[Input, Char] {
  def position: Position
  def offset: Int
  def array: ArrayCharSequence
  def drop(amount: Int): Input
  def remaining = array.length() - offset

  def move(increase: Int): Position = {
    var column = position.character
    var row = position.line
    for(index <- offset.until(offset + increase)) {
      val character = array.charAt(index)
      if (character == '\n') {
        row += 1
        column = 0
      } else {
        column += 1
      }
    }
    Position(row, column)
  }
}
