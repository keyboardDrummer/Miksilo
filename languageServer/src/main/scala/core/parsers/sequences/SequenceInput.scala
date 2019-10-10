package core.parsers.sequences

import core.parsers.core.ParseInput
import languageServer.Position

trait SequenceInput[Input, Elem] extends ParseInput {
  def head: Elem
  def tail: Input

  def drop(amount: Int): Input
  def safeIncrement(): Input =
    if (atEnd) this.asInstanceOf[Input]
    else drop(1)
  def end: Input
  def printRange(end: Input): String
  def position: Position
}
