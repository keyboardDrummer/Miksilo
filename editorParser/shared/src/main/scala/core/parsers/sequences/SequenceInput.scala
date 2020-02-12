package core.parsers.sequences

import core.parsers.core.ParseInput
import core.parsers.editorParsers.Position

trait SequenceInput[Input, Elem] extends ParseInput[Input] {
  def head(array: ArrayCharSequence): Elem
  def tail(array: ArrayCharSequence): Input

  def safeIncrement(array: ArrayCharSequence): Input =
    if (atEnd(array)) this.asInstanceOf[Input]
    else drop(array, 1)
  def end(array: ArrayCharSequence): Input
  def printRange(array: ArrayCharSequence, end: Input): String
  def position: Position
}
