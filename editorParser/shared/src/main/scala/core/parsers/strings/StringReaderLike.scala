package core.parsers.strings

import core.parsers.core.ParseInput
import core.parsers.editorParsers.Position
import core.parsers.sequences.SequenceInput

trait StringReaderLike[Input] extends SequenceInput[Input, Char] {
  def position: Position
  def offset: Int

  def drop(array: ArrayCharSequence, amount: Int): Input
  def remaining(array: ArrayCharSequence) = array.length() - offset

  def movePosition(array: ArrayCharSequence, increase: Int): Position = {
    if (increase < 0) {
      return decreasePosition(array, -increase)
    }

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

  def decreasePosition(array: ArrayCharSequence, decrease: Int): Position = {
    var column = position.character
    var row = position.line
    for(index <- (offset - 1).to(offset - decrease, -1)) {
      val character = array.charAt(index)
      if (character == '\n') {
        row -= 1
        column = 0
      } else {
        column -= 1
      }
    }
    Position(row, column)
  }
}
