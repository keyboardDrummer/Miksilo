package core.parsers.strings

import core.parsers.core.ParseText
import core.parsers.sequences.SequenceInput

trait StringReaderLike[Input] extends SequenceInput[Input, Char] {
  def offset: Int

  def drop(text: ParseText, amount: Int): Input
  def remaining(array: ParseText) = array.length() - offset

}
