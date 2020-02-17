package core.parsers.strings

import core.parsers.core.ParseText
import core.parsers.editorParsers.Position
import core.parsers.sequences.SequenceInput

trait StringReaderLike[Input] extends SequenceInput[Input, Char] {
  def position: Position
  def offset: Int

  def drop(array: ParseText, amount: Int): Input
  def remaining(array: ParseText) = array.length() - offset

  def movePosition(array: ParseText, increase: Int): Position = {
    if (increase < 0) {
      return decreasePosition(array, -increase)
    }

    var column = position.character
    var row = position.line
    for(index <- offset.until(offset + increase)) {
      if (index == array.length()) {
        column += 1
      } else {
        val character = array.charAt(index)
        if (character == '\n') {
          row += 1
          column = 0
        } else {
          column += 1
        }
      }
    }
    Position(row, column)
  }

  def decreasePosition(array: ParseText, decrease: Int): Position = {
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
