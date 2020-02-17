package core.parsers.sequences

import core.parsers.core.{ParseInput, ParseText}

trait SequenceInput[Input, Elem] extends ParseInput[Input] {
  def head(array: ParseText): Elem
  def tail(array: ParseText): Input

  def safeIncrement(array: ParseText): Input =
    if (atEnd(array)) this.asInstanceOf[Input]
    else drop(array, 1)
  def end(array: ParseText): Input
  def printRange(text: ParseText, end: Input): String
}
