package core.parsers.sequences

import core.parsers.core.ParseInput

trait SequenceInput[Input, Elem] extends ParseInput {
  def head: Elem
  def tail: Input
}
